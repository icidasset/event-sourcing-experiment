module Events where

import Data.Text.Lazy (Text)
import Flow
import Protolude hiding (Text)

import qualified Data.Aeson as Aeson
import qualified Database.Redis as Redis
import qualified Redis


-- Event


data Event payload = Event
    { payload :: payload
    , timestamp :: Integer
    , typ :: Text
    , version :: Integer
    }
    deriving (Generic, Show)



-- Event without payload


data NakedEvent = NakedEvent
    { timestamp :: Integer
    , typ :: Text
    , version :: Integer
    }
    deriving (Generic, Show)


instance Aeson.FromJSON NakedEvent


{-| Decode a Redis message into a NakedEvent,
    and then send the NE and the message to the `handler`.

    {!} Doesn't do anything if it cannot decode the message.
-}
getNaked :: (NakedEvent -> Redis.Message -> IO ()) -> Redis.Message -> IO ()
getNaked handler msg =
    msg
        |> Redis.decodeMessage
        |> map handler
        |> maybe mempty (\fn -> fn msg)
