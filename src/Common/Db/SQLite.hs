{-|
Description: @Transactor@ implementation for SQLite

This modules implements the @Transactor@ typeclass for dealing with a SQLite
db using @Database.SQLite.Simple@.

The implementation is as simple as possible. We open a new connection for each
transaction, closing it afterwards. The @SQLiteDb@ type merely holds the
@FilePath@ to the database. A practical implementation for a networked SQL
database such as Postgres could store a connection pool in its transactor type,
for example.

We define a newtype @SQLiteAction@, which is a simple wrapper around a function
that takes a @Connection@ and does some @IO@, presumably by using that
@Connection@ to talk to the database. The @Applicative@ and @Monad@ instances
for @SQLiteAction@ ensure that all the wrapped functions are called in
sequence.

Notice that this database allows us to run arbitrary code in the middle of a
database transaction (as most SQL databases do). We allow this here by making
@SQLiteAction@ an instance of @MonadIO@. If necessary, I believe we could make
it a monad transformer for full generality.
-}
module Common.Db.SQLite
  ( sqliteDb
  , SQLiteDb (..)
  , SQLiteAction(..)
  ) where

import Control.Monad.IO.Class
import Database.SQLite.Simple
import Common.Db.Transactor

newtype SQLiteDb = SQLiteDb FilePath

sqliteDb :: FilePath -> SQLiteDb
sqliteDb = SQLiteDb

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

instance MonadIO transactionM => Transactor SQLiteDb transactionM SQLiteAction where
  transact (SQLiteDb dbfile) (SQLiteAction act) = liftIO $ withConnection dbfile $ \conn -> withTransaction conn (act conn)

instance MonadIO SQLiteAction where
  liftIO ioAction = SQLiteAction $ const ioAction
