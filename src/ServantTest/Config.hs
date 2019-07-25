{-# LANGUAGE TemplateHaskell #-}

module ServantTest.Config where

import Data.Aeson.TH
import Data.Proxy
import Network.Wai.Handler.Warp (Port)
import Servant
import Control.Monad.Reader
import qualified Data.Text as T

import qualified Common.Config.Server as CS
import qualified Common.Config.Loader as CL
import Common.Version.Class (HasVersion(..))

data Config = Config { port :: Port
                     , version :: T.Text
                     , sqliteFile :: FilePath
                     }
  deriving (Eq, Show)

class HasConfig p where
  getConfig :: p -> Config

instance HasConfig Config where
  getConfig = id

instance HasVersion Config where
  getVersion = version

$(deriveJSON defaultOptions ''Config)

type API = CS.API Config

api :: Proxy API
api = Proxy

type ServerConstraints m c = (HasConfig c, CS.ServerConstraints m c)

server :: ServerConstraints m c => ServerT API m
server = asks getConfig

loadConfig :: IO Config
loadConfig = CL.loadConfig
