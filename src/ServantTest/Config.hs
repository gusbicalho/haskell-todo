{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module ServantTest.Config where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Network.Wai.Handler.Warp (Port)
import Servant
import Control.Monad.Reader
import qualified Data.Text as T

import qualified Common.Config.Server as CS
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

type API = CS.API Config

api :: Proxy API
api = Proxy

type ServerConstraints m c = (HasVal "config" c Config, CS.ServerConstraints m c)

server :: ServerConstraints m c => ServerT API m
server = asks $ #config

loadConfig :: IO Config
loadConfig = CL.loadConfig
