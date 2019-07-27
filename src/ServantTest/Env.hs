module ServantTest.Env where

import Servant.Auth.Server
import Crypto.JOSE (JWK)
import Common.Version.Class (HasVal(..), Version)
import ServantTest.Config (Config(..))
import ServantTest.Db.Transactor (Transactor (..))
import ServantTest.Db.SQLite (SqliteDb, sqliteDb)
import qualified ServantTest.Db.User as Db.User

data Env = Env {
  config :: Config
, sqlite :: SqliteDb
, jwtKey :: JWK
}

instance HasVal "transactor" SqliteDb Env where
  getVal = sqlite

instance HasVal "version" Version Env where
  getVal = getVal @"version" . config

instance HasVal "config" Config Env where
  getVal = config

instance HasVal "jwtKey" JWK Env where
  getVal = jwtKey

buildEnv :: Config -> IO Env
buildEnv config = do
  let dbfile = sqliteFile config
      sqlite = sqliteDb dbfile
  transact sqlite Db.User.initDB
  jwtKey <- generateKey
  return Env {
    config
  , sqlite
  , jwtKey
  }
