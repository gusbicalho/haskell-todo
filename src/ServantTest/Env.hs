{-# LANGUAGE OverloadedLabels #-}

module ServantTest.Env where

import Servant.Auth.Server
import Network.Wai.Handler.Warp (Port)
import Common.Version.Class (HasVal(..), Version)
import ServantTest.Config (Config(..))
import ServantTest.Db.Transactor (Transactor (..))
import ServantTest.Db.SQLite (SqliteDb, sqliteDb)
import qualified ServantTest.Db.User as Db.User
import qualified ServantTest.Db.Item as Db.Item

data Env = Env {
  config :: Config
, sqlite :: SqliteDb
, jwtSettings :: JWTSettings
, cookieSettings :: CookieSettings
}

instance HasVal "transactor" Env SqliteDb where
  getVal = sqlite

instance HasVal "version" Env Version where
  getVal = #version . config

instance HasVal "config" Env Config where
  getVal = config

instance HasVal "jwtSettings" Env JWTSettings where
  getVal = jwtSettings

instance HasVal "cookieSettings" Env CookieSettings where
  getVal = cookieSettings

instance HasVal "port" Env Port where
  getVal = port . config

cookieSettingsFromConfig :: Config -> CookieSettings
cookieSettingsFromConfig config =
    defaultCookieSettings { cookieIsSecure
                          , cookieSameSite = SameSiteStrict
                          }
  where cookieIsSecure | insecureAuthCookie config = NotSecure
                       | otherwise = Secure

buildEnv :: Config -> IO Env
buildEnv config = do
  let dbfile = sqliteFile config
      sqlite = sqliteDb dbfile
  jwtKey <- readKey $ jwtKeyPath config
  let jwtSettings = defaultJWTSettings jwtKey
      cookieSettings = cookieSettingsFromConfig config
  transact sqlite Db.User.initDB
  transact sqlite Db.Item.initDB
  return Env {
    config
  , sqlite
  , jwtSettings
  , cookieSettings
  }
