module Subjects where

import Events (NakedEvent(..))
import Flow
import Protolude

import qualified Data.Aeson as Aeson
import qualified Database.Redis as Redis
import qualified Redis
import qualified Subjects.User


{-| Pattern match to do something with the incoming event.
    A `NakedEvent` is an `Event` without a payload.
-}
handleEvent :: NakedEvent -> Redis.Message -> IO ()
handleEvent NakedEvent { typ = "CREATE_USER" } = tryDoing Subjects.User.create
handleEvent _ = const mempty


{-| Decode the Redis message based on a "handler" function,
    and then run the handler function with the result.
-}
tryDoing :: Aeson.FromJSON a => (a -> IO ()) -> Redis.Message -> IO ()
tryDoing fn = Redis.decodeMessage .> maybe mempty fn
