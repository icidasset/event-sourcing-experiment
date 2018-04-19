module Database where

import Flow
import Protolude

import qualified Data.String as String (fromString)
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Database.PostgreSQL.Simple as Postgres


-- 🗺


type Queries = [ Lazy.Text ]


data Migration = Migration
    { queries :: Queries }
    deriving (Generic, Show)



-- ⚡️


connect :: IO Postgres.Connection
connect =
    -- Establish a PostgreSQL connection
    -- Define environment variables according to the following:
    -- https://www.postgresql.org/docs/9.5/static/libpq-envars.html
    Postgres.connectPostgreSQL "dbname=event-sourcing-experiment"


disconnect :: Postgres.Connection -> IO ()
disconnect =
    Postgres.close



-- 🛳


migrate :: Migration -> IO ()
migrate migration = do
    conn <- connect

    -- Execute all queries sequentially,
    -- rolls back all of them if any fails.
    migration
        |> queries
        |> map Text.Lazy.unpack
        |> execute conn

    -- Close connection
    disconnect conn



-- 🚀


{-| Execute a bunch of queries in a transaction.
-}
execute :: Postgres.Connection -> [ [ Char ] ] -> IO ()
execute conn queries =
    queries
        |> map (String.fromString .> Postgres.execute_ conn)
        |> sequence
        |> map (const ())
        |> Postgres.withTransaction conn
