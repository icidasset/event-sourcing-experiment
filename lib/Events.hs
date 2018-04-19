module Events where

import Data.Text.Lazy (Text)
import Protolude (Generic, Integer, Maybe, Show)

import qualified Data.Aeson as Aeson


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
