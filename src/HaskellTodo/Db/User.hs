{-|
Description: Db actions for dealing with Users

This defines a typeclass 'UserDb', listing all actions one can use to deal with
'User's in a database. This module also defines an instance of such class for
the 'SQLiteAction' type: in other words, an implementation of 'UserDb' methods
returning values that can be executed by a SQLite 'Transactor'.

This implementation requires converting from 'User' to the SQLite types and
vice versa. The easiest way to do this was by implementing some typeclasses
from "Database.SQLite.Simple". To avoid orphan instances, we wrapped the types
from "HaskellTodo.Models.User" ins @newtype@s.

Compare this approach to the one used in our HTTP layer
("HaskellTodo.WireTypes.User"). There, we built completely separate types with
instances for Aeson's @ToJSON@ and @FromJSON@, and used dedicated functions
(from "HaskellTodo.Adapters.User") to convert between our models and the API
types.

Part of the reason for this difference was to test and demonstrate the two
approaches. However, I also believe that decoupling is most important when
dealing with and exposing external APIs, which are contracts between separate
applications. We usually have way more control over our own database than over
clients of our API (or over APIs we consume).

I believe this means it is acceptable, in this case, to take the more
lightweight approach when dealing with the database, while using the more
heavy, boilerplate-y approach for our HTTP API.
-}
module HaskellTodo.Db.User
  ( UserDb(..)
  ) where

import Prelude hiding (id)
import Data.Maybe (fromJust, listToMaybe)
import Data.String (fromString)
import Database.SQLite.Simple

import Common.Db.SQLite
import qualified HaskellTodo.Models.User as M.User
import HaskellTodo.Models.User ( User(..)
                               , NewUser(..)
                               , Login
                               , textToLogin
                               , textToPassword
                               , loginToText
                               , passwordToText
                               )

class UserDb action where
  initDB :: action ()
  listUsers :: action [User]
  getUser :: Integer -> action (Maybe User)
  createUser :: M.User.NewUser -> action User
  findUserByLogin :: M.User.Login -> action (Maybe User)

newtype DbUser = DbUser { dbToUser :: User }
instance FromRow DbUser where
  fromRow = DbUser <$> user
    where user = User <$> id <*> login <*> password
          id = field
          login = textToLogin <$> field
          password = textToPassword <$> field

newtype DbNewUser = DbNewUser NewUser
instance ToRow DbNewUser where
  toRow (DbNewUser (NewUser newLogin newPassword)) =
    toRow ( loginToText newLogin
          , passwordToText newPassword
          )

instance UserDb SQLiteAction where
  initDB :: SQLiteAction ()
  initDB = SQLiteAction $ \conn ->
    execute_ conn $
      fromString $ "CREATE TABLE IF NOT EXISTS users "
                ++ "( id integer not null primary key"
                ++ ", login text not null unique"
                ++ ", password text not null"
                ++ ")"

  listUsers :: SQLiteAction [User]
  listUsers = SQLiteAction listUsers'

  getUser :: Integer -> SQLiteAction (Maybe User)
  getUser rowId = SQLiteAction $ getUser' rowId

  createUser :: M.User.NewUser -> SQLiteAction User
  createUser newUser = SQLiteAction $ createUser' newUser

  findUserByLogin :: M.User.Login -> SQLiteAction (Maybe User)
  findUserByLogin login = SQLiteAction $ findUserByLogin' login

listUsers' :: Connection -> IO [User]
listUsers' conn = do
  results <- query conn "SELECT id, login, password FROM users" ()
  return . map dbToUser $ results

getUser' :: Integer -> Connection -> IO (Maybe User)
getUser' rowId conn = do
  results <- query conn "SELECT id, login, password FROM users WHERE id = ?" [rowId]
  let maybeDbUser = listToMaybe results
      maybeUser = dbToUser <$> maybeDbUser
  return maybeUser

createUser' :: M.User.NewUser -> Connection -> IO User
createUser' newUser conn = do
  execute conn "INSERT INTO users (login, password) values (?, ?)" (DbNewUser newUser)
  rowId <- lastInsertRowId conn
  fromJust <$> getUser' (fromIntegral rowId) conn

findUserByLogin' :: Login -> Connection -> IO (Maybe User)
findUserByLogin' login conn = do
  results <- query conn "SELECT id, login, password FROM users WHERE login = ?" [loginToText login]
  let maybeDbUser = listToMaybe results
      maybeUser = dbToUser <$> maybeDbUser
  return maybeUser
