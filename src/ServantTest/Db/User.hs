{-# LANGUAGE RecordWildCards #-}

module ServantTest.Db.User
  ( UserDb(..)
  ) where

import Prelude hiding (id)
import Data.Maybe (fromJust, listToMaybe)
import Database.SQLite.Simple

import ServantTest.Db.SQLite
import qualified ServantTest.Models.User as M.User
import ServantTest.Models.User (User(..), NewUser(..))

newtype DbUser = DbUser { dbToUser :: User }
instance FromRow DbUser where
  fromRow = DbUser <$> user
    where user = User <$> field <*> field <*> field <*> field

newtype DbNewUser = DbNewUser NewUser
instance ToRow DbNewUser where
  toRow (DbNewUser NewUser {..}) = toRow ( newName
                                         , newAge
                                         , newEmail
                                         )

class UserDb statement where
  initDB :: statement ()
  listUsers :: statement [User]
  getUser :: Integer -> statement (Maybe User)
  createUser :: M.User.NewUser -> statement User
  deleteUser :: Integer -> statement (Maybe User)

instance UserDb SQLiteAction where
  initDB :: SQLiteAction ()
  initDB = SQLiteAction $ \conn ->
    execute_ conn
      "CREATE TABLE IF NOT EXISTS users (id integer not null primary key, name text not null, age int not null, email text not null)"

  listUsers :: SQLiteAction [User]
  listUsers = SQLiteAction listUsers'

  getUser :: Integer -> SQLiteAction (Maybe User)
  getUser rowId = SQLiteAction $ getUser' rowId

  createUser :: M.User.NewUser -> SQLiteAction User
  createUser newUser = SQLiteAction $ createUser' newUser

  deleteUser :: Integer -> SQLiteAction (Maybe User)
  deleteUser rowId = SQLiteAction $ deleteUser' rowId

listUsers' :: Connection -> IO [User]
listUsers' conn = do
  results <- query conn "SELECT id, name, age, email FROM users" ()
  return . map dbToUser $ results

getUser' :: Integer -> Connection -> IO (Maybe User)
getUser' rowId conn = do
  results <- query conn "SELECT id, name, age, email FROM users WHERE id = ?" [rowId]
  let maybeDbUser = listToMaybe results
      maybeUser = dbToUser <$> maybeDbUser
  return maybeUser

createUser' :: M.User.NewUser -> Connection -> IO User
createUser' newUser conn = do
    execute conn "INSERT INTO users (name, age, email) values (?, ?, ?)" (DbNewUser newUser)
    rowId <- lastInsertRowId conn
    fromJust <$> getUser' (fromIntegral rowId) conn

deleteUser' :: Integer -> Connection -> IO (Maybe User)
deleteUser' rowId conn = do
  maybeUser <- getUser' rowId conn
  case maybeUser of
    Nothing -> return ()
    Just _  -> execute conn "DELETE FROM users WHERE id = ?" [rowId]
  return maybeUser
