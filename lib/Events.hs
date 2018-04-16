module Events where

import Data.Text.Lazy (Text)
import Protolude (Generic, Integer, Maybe, Show)


-- Event


data Event payload = Event
    { payload :: payload
    , timestamp :: Integer
    , typ :: Text
    , version :: Integer
    }
    deriving (Generic, Show)
