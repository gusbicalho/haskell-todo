{-# LANGUAGE TemplateHaskell #-}

module ServantTest.Config where

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Network.Wai.Handler.Warp (Port)
import Servant
import Control.Monad.Reader
import Data.String (IsString(..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Common.Config.Server as CS
import qualified Common.Config.Loader as CL
import Common.Version.Class (HasVal(..), Version, fromText)

newtype JWTSecret = JWTSecret { getSecret :: B8.ByteString } deriving (Eq, Show)
instance FromJSON JWTSecret where
  parseJSON v = JWTSecret . B8.pack <$> parseJSON v
instance ToJSON JWTSecret where
  toJSON _ = toJSON $ String "REDACTED"
instance IsString JWTSecret where
  fromString = JWTSecret . fromString

data Config = Config { port :: Port
                     , version :: T.Text
                     , sqliteFile :: FilePath
                     , jwtSecret :: JWTSecret
                     }
  deriving (Eq, Show)

instance HasVal "config" Config Config where
  getVal = id

instance HasVal "version" Version Config where
  getVal = fromText . version

$(deriveJSON defaultOptions ''Config)

type API = CS.API Config

api :: Proxy API
api = Proxy

type ServerConstraints m c = (HasVal "config" Config c, CS.ServerConstraints m c)

server :: ServerConstraints m c => ServerT API m
server = asks $ getVal @"config"

loadConfig :: IO Config
loadConfig = CL.loadConfig
