module Redis where

import Flow
import Protolude (Maybe(..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Database.Redis as Redis


decodeMessage :: Aeson.FromJSON a => Redis.Message -> Maybe a
decodeMessage msg =
    msg
        |> Redis.msgMessage
        |> ByteString.Lazy.fromStrict
        |> Aeson.decode
