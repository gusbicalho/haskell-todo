module Common.Db.SQLite
  ( sqliteDb
  , SqliteDb (..)
  , SQLiteAction(..)
  ) where

import Control.Monad.IO.Class
import Database.SQLite.Simple
import Common.Db.Transactor

newtype SqliteDb = SqliteDb FilePath

sqliteDb :: FilePath -> SqliteDb
sqliteDb = SqliteDb

newtype SQLiteAction a = SQLiteAction { action :: Connection -> IO a } deriving (Functor)

instance Applicative SQLiteAction where
  pure x = SQLiteAction $ \_ -> return x
  (SQLiteAction f) <*> (SQLiteAction x) = SQLiteAction $ \conn -> do
    f' <- f conn
    x' <- x conn
    return $ f' x'

instance Monad SQLiteAction where
  return = pure
  (SQLiteAction act) >>= f = SQLiteAction $ \conn -> do
    actResult <- act conn
    let nextAct = action $ f actResult
    nextAct conn

instance MonadIO transactionM => Transactor SqliteDb transactionM SQLiteAction where
  transact (SqliteDb dbfile) (SQLiteAction act) = liftIO $ withConnection dbfile $ \conn -> withTransaction conn (act conn)

instance MonadIO SQLiteAction where
  liftIO ioAction = SQLiteAction $ const ioAction
