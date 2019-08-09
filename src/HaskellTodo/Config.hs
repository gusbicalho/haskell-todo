{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description: Concrete Config type for the service, along with a ops server.

Defines a concrete Config type, as well as a simple Servant JSON API to get
current config for the service. The Servant API expects a Reader monad that
provides some type which contains a Config (accessible via HasVal "config").

If we had to, we could use a custom ToJSON instance or and adapter fn here to
redact sensitive fields.
-}
module HaskellTodo.Config where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Network.Wai.Handler.Warp (Port)
import Servant
import Control.Monad.Reader
import qualified Data.Text as T

import qualified Common.Config.Loader as CL
import Common.Version.Class (HasVal(..), Version, fromText)

data Config = Config { port :: Port
                     , version :: T.Text
                     , sqliteFile :: FilePath
                     , jwtKeyPath :: FilePath
                     , insecureAuthCookie :: Bool
                     }
  deriving (Eq, Show)

instance HasVal "config" Config Config where
  getVal = id

instance HasVal "version" Config Version where
  getVal = fromText . version

instance HasVal "port" Config Port where
  getVal = port

$(deriveJSON defaultOptions ''Config)

type API = "dump" :> Get '[JSON] Config

api :: Proxy API
api = Proxy

type ServerConstraints m c = (HasVal "config" c Config, MonadReader c m)

server :: ServerConstraints m c => ServerT API m
server = asks #config

loadConfig :: IO Config
loadConfig = CL.loadConfig
