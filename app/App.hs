module Main where

import Events (NakedEvent(..))
import Flow
import Prelude (read)
import Protolude hiding (ByteString)
import Web.Scotty as Scotty

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Database.Redis as Redis
import qualified Redis
import qualified Subjects.User
import qualified Time


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO ()
main = do

    -- Redis
    -- -----

    conn <- Redis.checkedConnect Redis.defaultConnectInfo

    -- Subscribe to channel and listen for messages.
    -- {!} Asynchronous IO

    (onMessage conn)
        |> Redis.pubSub Redis.subscription
        |> Redis.runRedis conn
        |> async


    -- Continuation
    -- ------------
    -- This goes through all the unprocessed events,
    -- in case our app was offline for a bit.

    let run = Redis.run conn
    let fif = map (BC8.unpack .> read .> \x -> x + 1) .> fromMaybe 0
    let eif = \x -> x - 1

    fromIndex   <- map fif (run Nothing $ Redis.get Redis.hashStorageKey)
    endIndex    <- map eif (run 0       $ Redis.llen Redis.eventStreamKey)
    events      <-         (run []      $ Redis.lrange Redis.eventStreamKey fromIndex endIndex)

    -- Go through all the unprocessed events,
    -- and run `eventHandler` for each one.
    events
        |> map (Redis.msgFromString .> getNaked)
        |> sequence

    -- Update last-received list-index,
    -- as a result from the continuation above.
    endIndex
        |> show
        |> Redis.set Redis.hashStorageKey
        |> Redis.runRedis conn


    -- Scotty
    -- ------

    scotty 4567 $ do

        -- POST /users
        -- Creates a user record.
        --
        post "/users" (createUser conn)



-- Subscriptions


{-| Function that will be called when we receive a message from the channel.
    Decodes the message into a `NakedEvent`, which is the same as `Event`,
    but without a payload. Then uses the `eventHandler` function below to
    actually do something with the event.
-}
onMessage :: Redis.Connection -> Redis.Message -> IO Redis.PubSub
onMessage conn msg = do

    -- Get the list index of this message.
    -- Which is the length of the stream minus one.
    index <- Redis.eventStreamKey
                |> Redis.llen
                |> Redis.run conn 0
                |> map (\x -> x - 1)

    -- Also store the list index of this message.
    -- So we know what message was last received.
    index
        |> show
        |> Redis.set Redis.hashStorageKey
        |> Redis.runRedis conn

    -- Do something with the event.
    -- {!} Asynchronous IO
    msg
        |> getNaked
        |> async

    -- ✌️
    return mempty


{-| Decode the Redis message into a NakedEvent,
    and then send the NE and the message to the `eventHandler`.
-}
getNaked :: Redis.Message -> IO ()
getNaked msg =
    msg
        |> Redis.decodeMessage
        |> map eventHandler
        |> maybe mempty (\fn -> fn msg)


{-| Pattern match to do something with the incoming event.
    A `NakedEvent` is an `Event` without a payload.
-}
eventHandler :: NakedEvent -> Redis.Message -> IO ()
eventHandler NakedEvent { typ = "CREATE_USER" } = tryDoing Subjects.User.create
eventHandler _ = const mempty


{-| Decode the Redis message based on a "handler" function,
    and then run the handler function with the result.
-}
tryDoing :: Aeson.FromJSON a => (a -> IO ()) -> Redis.Message -> IO ()
tryDoing fn = Redis.decodeMessage .> maybe mempty fn



-- Scotty


beamMeUpScotty :: MonadIO m => IO a -> m a
beamMeUpScotty = liftIO


createUser :: Redis.Connection -> Scotty.ActionM ()
createUser conn = do

    e <- param "email"
    t <- beamMeUpScotty Time.currentUnixTime

    -- Create event
    let event = Subjects.User.creationEvent e t

    -- Send event to Redis
    event
        |> Aeson.encode
        |> ByteString.Lazy.toStrict
        |> sendEvent conn
        |> beamMeUpScotty

    -- Response
    Scotty.text "{ \"data\": { \"authToken\": \"example\" } }"



-- Notify


sendEvent :: Redis.Connection -> Strict.ByteString -> IO ()
sendEvent conn event = do
    [ event ]
        |> Redis.rpush Redis.eventStreamKey
        |> Redis.runRedis conn

    event
        |> Redis.publish Redis.channel
        |> Redis.runRedis conn

    return ()
