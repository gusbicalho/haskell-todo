{-|
Description: Define and build 'Env' type for this application

This module defines the 'Env' type, which is to be used as global configuration
and reference holder for the app. We don't use it literally a global variable.
Instead, we call 'buildEnv' in @main@ and pass around the resulting value.

Code in @HaskellTodo.Controller.*@ modules typically depend on an abstract
'env' type, with some constraints using the 'HasField' typeclass. This allows
us to unit test controllers without building an actual 'Env'.

On the other hand, code in the @HaskellTodo.HttpApi.*@ modules depend on 'Env'
directly. If we had other entry ports in this service (such as a message
consumer), I think I'd also import 'Env' explicitly in them. I think this makes
sense for two reasons:

1) These ports should usually be very simple, just some glue code between
   adapters, controllers, and library code. I feel like it isn't very
   productive to unit test this kind of code - it is better covered by good
   integration tests, which should be able to build an 'Env' value.

2) Polymorphic constraints add a lot of noise, especially when we combine
   several server functions with different constraints, depending on which
   controllers they call. The main advantage of using abstract types would be
   to allow unit testing, and 1 tells us that's not worth it.

I'm not strongly attached to this position, which means I might change my mind
in the future, or depending on the nature of a project.
-}
module HaskellTodo.Env (
    Env (..)
  , buildEnv
  ) where

import Servant.Auth.Server
import Network.Wai.Handler.Warp (Port)
import Crypto.BCrypt
import Common.HasField
import Common.Crypto.BCrypt
import Common.Version.Types (Version)
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
      bcrypt = BCrypter { bcryptPolicy = fastBcryptHashingPolicy }
  transact sqlite Db.User.initDB
  transact sqlite Db.Item.initDB
  return Env {
    config
  , sqlite
  , jwtSettings
  , cookieSettings
  , bcrypt
  }
