module ServantTest.Env where

import Control.Monad.IO.Class
import Common.Version.Class (HasVersion(..))
import ServantTest.Config (Config(..), HasConfig(..))
import ServantTest.Sqlite (Transactor (..), HasTransactor (..), UserDb(..), SqliteDb, sqliteDb)

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
  transact sqlite initDB
  return Env {
    config
  , sqlite
  }
