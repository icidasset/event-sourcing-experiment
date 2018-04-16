module Database where

import Dhall (Text)
import Flow
import Protolude

import qualified Data.String as String (fromString)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Database.PostgreSQL.Simple as Postgres


-- 🗺


type Queries = [ Dhall.Text ]


data Migration = Migration
    { queries :: Queries }
    deriving (Generic, Show)



-- 🛳


migrate :: Migration -> IO ()
migrate migration = do
    -- Establish a PostgreSQL connection
    -- Define environment variables according to the following:
    -- https://www.postgresql.org/docs/9.5/static/libpq-envars.html
    conn <- Postgres.connectPostgreSQL "dbname=event-sourcing-experiment"

    -- Execute all queries sequentially
    sequence $ map (execute conn) (queries migration)

    -- Close connection
    Postgres.close conn



-- 🚀


execute :: Postgres.Connection -> Dhall.Text -> IO Int64
execute conn query =
    query
        |> Text.Lazy.unpack
        |> String.fromString
        |> Postgres.execute_ conn
