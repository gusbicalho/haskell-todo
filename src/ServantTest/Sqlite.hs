module ServantTest.Sqlite where

import Prelude hiding (id)
import Database.SQLite.Simple

import qualified ServantTest.Models.User as M.User

newtype DbUser = DbUser { dbToUser :: M.User.User }
instance FromRow DbUser where
  fromRow = DbUser . toUser <$> fromRow
    where toUser        (        id,        name,        age,        email ) =
            M.User.User { M.User.id, M.User.name, M.User.age, M.User.email }

-- V1: call db fns directly
-- V2: create a type to hold the db setting, use typeclass for operations, put evidence in Env
-- V3: create a API (typeclass) with actions in STM to communicate with a DB worker thread

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS users (id integer not null primary key, name text not null, age int not null, email text not null)"

transact :: FilePath -> (Connection -> IO a) -> IO a
transact dbfile act = withConnection dbfile $ \conn -> withTransaction conn (act conn)

listUsers :: Connection -> IO [M.User.User]
listUsers conn = do
    results <- query conn "select * from users" ()
    return . map dbToUser $ results
