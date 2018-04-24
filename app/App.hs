module Main where

import Events (NakedEvent(..))
import Flow
import Protolude hiding (ByteString)
import Web.Scotty as Scotty

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Aeson as Aeson
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
    -- This happens asynchronously, so we can use scotty as well.
    (onMessage conn)
        |> Redis.pubSub Redis.subscription
        |> Redis.runRedis conn
        |> async


    -- TODO
    -- ----

    -- Check if we missed anything from the event stream.
    -- The application might have been offline while events were published.
    -- Decrypt the stored MD5 hash to see what the last received event was.


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
    -- Also store the MD5 hash of this message.
    -- So we know what message was last received.
    msg
        |> Redis.msgMessage
        |> MD5.hash
        |> Redis.set Redis.hashStorageKey
        |> Redis.runRedis conn

    -- Do something with the event.
    -- {!} Asynchronous IO
    msg
        |> Redis.decodeMessage
        |> map eventHandler
        |> maybe mempty (\fn -> fn msg)
        |> async

    -- ✌️
    return mempty


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
    let event   = Subjects.User.creationEvent e t
    let msg     = ByteString.Lazy.toStrict (Aeson.encode event)

    -- Send event to Redis
    [ msg ]
        |> Redis.rpush Redis.eventStreamKey
        |> Redis.runRedis conn
        |> beamMeUpScotty

    msg
        |> Redis.publish Redis.channel
        |> Redis.runRedis conn
        |> beamMeUpScotty

    -- Response
    Scotty.text "{ \"data\": { \"authToken\": \"example\" } }"
