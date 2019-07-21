{-# LANGUAGE TemplateHaskell #-}

module ServantTest.Config where

import Data.Aeson.TH
import Data.Proxy
import Network.Wai.Handler.Warp (Port)
import Servant
import Control.Monad.Reader

import qualified Common.Config.Server as CS
import qualified Common.Config.Loader as CL
import Common.Version.Class (HasVersion(..))

data Config = Config { port :: Port
                     , version :: String
                     }
  deriving (Eq, Show)

instance HasVersion Config where
  getVersion = version

$(deriveJSON defaultOptions ''Config)

type API = CS.API Config

api :: Proxy API
api = Proxy

type ServerConstraints m = CS.ServerConstraints m Config

server :: ServerConstraints m => ServerT API m
server = ask

loadConfig :: IO Config
loadConfig = CL.loadConfig
