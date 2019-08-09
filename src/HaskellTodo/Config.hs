{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description: Concrete Config type for the service, along with a ops server.

Defines a concrete Config type, as well as a simple Servant JSON API to get
current config for the service. The Servant API expects a Reader monad that
provides some type which contains a Config (accessible via HasField "config").

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
import Common.Util.AesonHelpers

import qualified Common.Config.Loader as CL
import Common.HasField
import Common.Version.Types (Version, fromText)

data Config = Config { port :: Port
                     , _version :: T.Text
                     , sqliteFile :: FilePath
                     , jwtKeyPath :: FilePath
                     , insecureAuthCookie :: Bool
                     }
  deriving (Eq, Show)

instance HasField "config" Config Config where
  getField = id

instance HasField "version" Config Version where
  getField = fromText . _version

$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''Config)

type API = "dump" :> Get '[JSON] Config

api :: Proxy API
api = Proxy

type ServerConstraints m c = (HasField "config" c Config, MonadReader c m)

server :: ServerConstraints m c => ServerT API m
server = asks #config

loadConfig :: IO Config
loadConfig = CL.loadConfig
