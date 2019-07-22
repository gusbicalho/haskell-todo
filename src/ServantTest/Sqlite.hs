module ServantTest.Sqlite
  ( initDB
  , transact
  , listUsers
  , getUser
  , createUser
  ) where

import Prelude hiding (id)
import Database.SQLite.Simple
import Data.Maybe (fromJust, listToMaybe)

import qualified ServantTest.Models.User as M.User
import ServantTest.Models.User (User(..), NewUser(..))

newtype DbUser = DbUser { dbToUser :: User }
instance FromRow DbUser where
  fromRow = DbUser . toUser <$> fromRow
    where toUser ( id, name, age, email ) =
            User { id, name, age, email }

-- V1: call db fns directly
-- V2: create a type to hold the db setting, use typeclass for operations, put evidence in Env
-- V3: create a API (typeclass) with actions in STM to communicate with a DB worker thread

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS users (id integer not null primary key, name text not null, age int not null, email text not null)"

transact :: FilePath -> (Connection -> IO a) -> IO a
transact dbfile act = withConnection dbfile $ \conn -> withTransaction conn (act conn)

listUsers :: Connection -> IO [User]
listUsers conn = do
  results <- query conn "SELECT id, name, age, email FROM users" ()
  return . map dbToUser $ results

getUser :: Integer -> Connection -> IO (Maybe User)
getUser rowId conn = do
  results <- query conn "SELECT id, name, age, email FROM users WHERE id = ?" [rowId]
  let maybeDbUser = listToMaybe results
      maybeUser = dbToUser <$> maybeDbUser
  return maybeUser

createUser :: M.User.NewUser -> Connection -> IO User
createUser newUser conn = do
    execute conn "INSERT INTO users (name, age, email) values (?, ?, ?)" (userTuple newUser)
    rowId <- lastInsertRowId conn
    fromJust <$> getUser (fromIntegral rowId) conn
  where
    userTuple (NewUser { newName, newAge, newEmail }) = (newName, newAge, newEmail)
