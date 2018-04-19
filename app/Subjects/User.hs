module Subjects.User where

import Database.PostgreSQL.Simple (Only(..))
import Events (Event(..))
import Flow
import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.String as String (fromString)
import qualified Data.Text as Text
import qualified Database
import qualified Database.PostgreSQL.Simple as Postgres


-- User


data User =
    User
        { id :: Integer
        , email :: Text
        , created_at :: Text
        } deriving (Generic, Show)


instance Aeson.FromJSON User
instance Aeson.ToJSON User



-- User before inserted into database


data UserRaw =
    UserRaw
        { email :: Text
        } deriving (Generic, Show)


instance Aeson.FromJSON UserRaw
instance Aeson.FromJSON (Event UserRaw)
instance Aeson.ToJSON UserRaw
instance Aeson.ToJSON (Event UserRaw)



-- Events


creationEvent :: Text -> Integer -> Event UserRaw
creationEvent e t =
    Event
        { payload = UserRaw { email = e }
        , timestamp = t
        , typ = "CREATE_USER"
        , version = 1
        }



-- Handlers


create :: Event UserRaw -> IO ()
create event = do
    conn <- Database.connect

    -- User creation query
    let q = "INSERT INTO users (email) VALUES (?) RETURNING id, created_at"
    let u = payload event
    let e = email (u :: UserRaw)

    -- Create user and get the `id` and `created_at`
    ((id :: Integer, created_at :: Text) : _) <- Postgres.query conn q (Only e)

    -- Close connection
    Database.disconnect conn
