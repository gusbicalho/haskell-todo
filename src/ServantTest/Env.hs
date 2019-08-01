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

instance HasVal "transactor" SqliteDb Env where
  getVal = sqlite

instance HasVal "version" Version Env where
  getVal = #version . config

instance HasVal "config" Config Env where
  getVal = config

instance HasVal "jwtSettings" JWTSettings Env where
  getVal = jwtSettings

instance HasVal "cookieSettings" CookieSettings Env where
  getVal = cookieSettings

instance HasVal "port" Port Env where
  getVal = port . config

cookieSettingsFromConfig :: Config -> CookieSettings
cookieSettingsFromConfig config =
    defaultCookieSettings { cookieIsSecure
                          , cookieXsrfSetting = Just defaultXsrfCookieSettings { xsrfExcludeGet = True }
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
