module ServantTest.Env where

import Common.Version.Class (HasVersion(..))
import ServantTest.Config (Config(..), HasConfig(..))
import ServantTest.Db.Transactor (Transactor (..), HasTransactor (..))
import ServantTest.Db.SQLite (SqliteDb, sqliteDb)
import qualified ServantTest.Db.User as Db.User

data Env = Env {
  config :: Config
, sqlite :: SqliteDb
}

instance HasTransactor Env SqliteDb where
  getTransactor = sqlite

instance HasVersion Env where
  getVersion = getVersion . config

instance HasConfig Env where
  getConfig = config

buildEnv :: Config -> IO Env
buildEnv config = do
  let dbfile = sqliteFile config
      sqlite = sqliteDb dbfile
  transact sqlite Db.User.initDB
  return Env {
    config
  , sqlite
  }
