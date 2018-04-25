module Redis where

import Flow
import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Database.Redis as Redis


-- Constants


channel :: Strict.ByteString
channel = "event-sourcing-experiment"


eventStreamKey :: Strict.ByteString
eventStreamKey = channel


hashStorageKey :: Strict.ByteString
hashStorageKey = "SYSTEM_IDENTIFIER_GOES_HERE"


subscription :: Redis.PubSub
subscription = Redis.subscribe [ channel ]



-- Subscriptions


decodeMessage :: Aeson.FromJSON a => Redis.Message -> Maybe a
decodeMessage msg =
    msg
        |> Redis.msgMessage
        |> ByteString.Lazy.fromStrict
        |> Aeson.decode



-- Shortcuts


run :: Redis.Connection -> a -> (Redis.Redis (Either e a)) -> IO a
run conn defaultValue action =
    action
        |> Redis.runRedis conn
        |> map (either (const defaultValue) identity)



-- Messages


msgFromString :: Strict.ByteString -> Redis.Message
msgFromString s =
    Redis.Message { Redis.msgChannel = "", Redis.msgMessage = s }
