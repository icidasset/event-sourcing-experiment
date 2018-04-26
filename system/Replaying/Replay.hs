module Main where

import Flow
import Protolude

import qualified Database.Redis as Redis
import qualified Events
import qualified Redis
import qualified Subjects


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO ()
main = do

    -- Redis
    -- -----

    conn <- Redis.checkedConnect Redis.defaultConnectInfo


    -- Replay
    -- ------

    events <- Redis.run conn [] $ Redis.lrange Redis.eventStreamKey 0 (-1)

    -- Go through all the events,
    -- and run `handleEvent` for each one.
    events
        |> map (Redis.msgFromString .> Events.getNaked Subjects.handleEvent)
        |> sequence

    -- Finish!
    return ()
