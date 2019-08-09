{-# LANGUAGE OverloadedLabels #-}

module HaskellTodo.Env where

import Servant.Auth.Server
import Network.Wai.Handler.Warp (Port)
import Crypto.BCrypt
import Common.HasField
import Common.Crypto.BCrypt
import Common.Version.Class (Version)
import HaskellTodo.Config (Config(..))
import Common.Db.Transactor (Transactor (..))
import Common.Db.SQLite (SQLiteDb, sqliteDb)
import qualified HaskellTodo.Db.User as Db.User
import qualified HaskellTodo.Db.Item as Db.Item

data Env = Env {
  config :: Config
, sqlite :: SQLiteDb
, jwtSettings :: JWTSettings
, cookieSettings :: CookieSettings
, bcrypt :: BCrypter
}

instance HasField "transactor" Env SQLiteDb where
  getField = sqlite

instance HasField "version" Env Version where
  getField = #version . config

instance HasField "port" Env Port where
  getField = port . config

instance HasField "hasher" Env BCrypter where
  getField = bcrypt

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
      bcrypt = BCrypter { bcryptPolicy = slowerBcryptHashingPolicy }
  transact sqlite Db.User.initDB
  transact sqlite Db.Item.initDB
  return Env {
    config
  , sqlite
  , jwtSettings
  , cookieSettings
  , bcrypt
  }
