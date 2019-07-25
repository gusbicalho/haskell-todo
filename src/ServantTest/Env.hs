module ServantTest.Env where

import Common.Version.Class (HasVal(..), Version)
import ServantTest.Config (Config(..))
import ServantTest.Db.Transactor (Transactor (..), HasTransactor (..))
import ServantTest.Db.SQLite (SqliteDb, sqliteDb)
import qualified ServantTest.Db.User as Db.User

data Env = Env {
  config :: Config
, sqlite :: SqliteDb
}

instance HasTransactor Env SqliteDb where
  getTransactor = sqlite

instance HasVal "version" Version Env where
  getVal = getVal @"version" . config

instance HasVal "config" Config Env where
  getVal = config

buildEnv :: Config -> IO Env
buildEnv config = do
  let dbfile = sqliteFile config
      sqlite = sqliteDb dbfile
  transact sqlite Db.User.initDB
  return Env {
    config
  , sqlite
  }
