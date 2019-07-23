module ServantTest.Sqlite
  ( Transactor (..)
  , HasTransactor (..)
  , UserDb (..)
  , SqliteDb ()
  , sqliteDb
  , SqliteStatement
  ) where

import Prelude hiding (id)
import Database.SQLite.Simple
import Data.Maybe (fromJust, listToMaybe)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

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

class Transactor t transactionM statement | t -> statement where
  transact :: t -> statement a -> transactionM a

class HasTransactor p t | p -> t where
  getTransactor :: p -> t

class UserDb statement where
  initDB :: statement ()
  listUsers :: statement [User]
  getUser :: Integer -> statement (Maybe User)
  createUser :: M.User.NewUser -> statement User
  deleteUser :: Integer -> statement (Maybe User)

newtype SqliteDb = SqliteDb { dbfile :: FilePath }

sqliteDb :: FilePath -> SqliteDb
sqliteDb = SqliteDb

newtype SqliteStatement a = SS { statement :: Connection -> IO a } deriving (Functor)

instance Applicative SqliteStatement where
  pure x = SS $ \_ -> return x
  (SS f) <*> (SS x) = SS $ \conn -> do
    f' <- f conn
    x' <- x conn
    return $ f' x'

instance Monad SqliteStatement where
  return = pure
  (SS act) >>= f = SS $ \conn -> do
    actResult <- act conn
    let nextAct = statement $ f actResult
    nextAct conn

instance MonadIO transactionM => Transactor SqliteDb transactionM SqliteStatement where
  transact (SqliteDb dbfile) (SS act) = liftIO $ withConnection dbfile $ \conn -> withTransaction conn (act conn)

instance UserDb SqliteStatement where
  initDB :: SqliteStatement ()
  initDB = SS $ \conn ->
    execute_ conn
      "CREATE TABLE IF NOT EXISTS users (id integer not null primary key, name text not null, age int not null, email text not null)"

  listUsers :: SqliteStatement [User]
  listUsers = SS listUsers'

  getUser :: Integer -> SqliteStatement (Maybe User)
  getUser rowId = SS $ getUser' rowId

  createUser :: M.User.NewUser -> SqliteStatement User
  createUser newUser = SS $ createUser' newUser

  deleteUser :: Integer -> SqliteStatement (Maybe User)
  deleteUser rowId = SS $ deleteUser' rowId

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
    execute conn "INSERT INTO users (name, age, email) values (?, ?, ?)" (userTuple newUser)
    rowId <- lastInsertRowId conn
    fromJust <$> getUser' (fromIntegral rowId) conn
  where
    userTuple (NewUser { newName, newAge, newEmail }) = (newName, newAge, newEmail)

deleteUser' :: Integer -> Connection -> IO (Maybe User)
deleteUser' rowId conn = do
  maybeUser <- getUser' rowId conn
  case maybeUser of
    Nothing -> return ()
    Just _  -> execute conn "DELETE FROM users WHERE id = ?" [rowId]
  return maybeUser

-- initDB :: FilePath -> IO ()
-- initDB dbfile = withConnection dbfile $ \conn ->
--   execute_ conn
--     "CREATE TABLE IF NOT EXISTS users (id integer not null primary key, name text not null, age int not null, email text not null)"

-- transact :: FilePath -> (Connection -> IO a) -> IO a
-- transact dbfile act = withConnection dbfile $ \conn -> withTransaction conn (act conn)

-- listUsers :: Connection -> IO [User]
-- listUsers conn = do
--   results <- query conn "SELECT id, name, age, email FROM users" ()
--   return . map dbToUser $ results

-- getUser :: Integer -> Connection -> IO (Maybe User)
-- getUser rowId conn = do
--   results <- query conn "SELECT id, name, age, email FROM users WHERE id = ?" [rowId]
--   let maybeDbUser = listToMaybe results
--       maybeUser = dbToUser <$> maybeDbUser
--   return maybeUser

-- createUser :: M.User.NewUser -> Connection -> IO User
-- createUser newUser conn = do
--     execute conn "INSERT INTO users (name, age, email) values (?, ?, ?)" (userTuple newUser)
--     rowId <- lastInsertRowId conn
--     fromJust <$> getUser (fromIntegral rowId) conn
--   where
--     userTuple (NewUser { newName, newAge, newEmail }) = (newName, newAge, newEmail)
