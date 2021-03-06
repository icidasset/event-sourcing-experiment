module Main where

import Events (Event(..))
import Flow
import Protolude
import Shikensu (Definition(..), Dictionary)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath (joinPath)

import qualified Control.Exception.Safe as Safe
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import qualified Data.Text.Lazy.IO as Text.Lazy
import qualified Database
import qualified Dhall
import qualified Shikensu
import qualified Shikensu.Contrib.IO as Shikensu


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO ()
main = do
    -- Make a list of all the events and the events in the ledger.
    events <- Shikensu.listRelativeF "." [ "system/Events/**/*.event" ] >>= Shikensu.read
    ledger <- Shikensu.listRelativeF "." [ "system/Ledger/**/*.json" ] >>= Shikensu.read

    -- Which are the events that are not yet in the ledger?
    let new = selectNew events ledger

    -- Create ledger directory if it doesn't exist yet.
    createDirectoryIfMissing True "./system/Ledger/"

    -- Execute events sequentially.
    -- If the execution was successful, add the event to the ledger.
    -- If not, stop sequence and show error.
    sequentialEventProcessor new



-- Selection


selectNew :: Dictionary -> Dictionary -> Dictionary
selectNew events ledger =
    let
        ledgerIds = map basename ledger
    in
        -- Only keep the events that are not in the ledger
        filter (\def -> List.notElem (basename def) ledgerIds) events



-- Payload


data Payload
    = DatabaseMigration { queries :: Database.Queries }
    | None
    deriving (Generic, Show)


instance Aeson.ToJSON (Event Payload)
instance Aeson.ToJSON Payload
instance Dhall.Interpret (Event Payload)
instance Dhall.Interpret Payload



-- IO


{-| Execute events sequentially.
    If the execution was successful, add the event to the ledger.
    If not, stop sequence and show error.
-}
sequentialEventProcessor :: Dictionary -> IO ()
sequentialEventProcessor =
    let
        log :: [ Char ] -> IO ()
        log = putStrLn

        fold acc [] = acc
        fold acc (def:rest) = do
            result <- Safe.tryAny (acc >> processEvent def)

            case result of
                Left exception ->
                    -- Show error message and stop sequence
                    log ("") >>
                    log (basename def <> extname def <> " produced an error: \x1b[31m") >>
                    log (displayException exception <> "\x1b[0m")

                Right value ->
                    -- Add event to ledger and carry on
                    fold (addEventToLedger def value) rest
    in
        fold $ return ()



{-| Decode the Dhall program into an `Event`,
    send it to the appropriate processor
    and encode the `Event` into a `Aeson.Value`.
-}
processEvent :: Definition -> IO Aeson.Value
processEvent Definition{ content, dirname, rootDirname, workingDirname } =
    let
        cwd =
            joinPath [ rootDirname, workingDirname, dirname ]
    in
        content
            |> decodeDhallProgram
            |> withCurrentDirectory cwd
            |> processPayload
            |> map Aeson.toJSON


{-| Decode the Dhall program into an Event.
-}
decodeDhallProgram :: Dhall.Interpret a => Maybe ByteString -> IO a
decodeDhallProgram content =
    content
        |> map Text.decodeUtf8
        |> map Text.Lazy.fromStrict
        |> Maybe.fromMaybe ""
        |> Dhall.input Dhall.auto
        |> Dhall.detailed


{-| Send an Event to the appropriate processor.
-}
processPayload :: IO (Event Payload) -> IO (Event Payload)
processPayload io = do
    event <- io

    case payload event of
        DatabaseMigration migration ->
            Database.migrate (Database.Migration migration)

        None ->
            return ()

    return event


{-| Adds an event to the Ledger.
-}
addEventToLedger :: Aeson.ToJSON a => Definition -> a -> IO ()
addEventToLedger Definition{ basename } event =
    event
        |> Aeson.encodePretty
        |> Text.Lazy.decodeUtf8
        |> Text.Lazy.writeFile ("./system/Ledger/" <> basename <> ".json")
