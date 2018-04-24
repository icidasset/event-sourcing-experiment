module Time where

import Flow ((.>))
import Protolude

import qualified Data.UnixTime as UnixTime (UnixTime(..), getUnixTime)


{-| Get the current unix time in seconds.
-}
currentUnixTime :: IO Integer
currentUnixTime =
    map (UnixTime.utSeconds .> fromEnum .> toInteger) UnixTime.getUnixTime
