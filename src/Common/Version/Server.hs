{-# LANGUAGE TemplateHaskell #-}

module Common.Version.Server
  ( api
  , server
  , API
  , ServerConstraints
  , HasVersion(..)
  ) where

import Control.Monad.Reader
import Data.Proxy
import Servant
import Data.Aeson.TH
import qualified Data.Text as T
import Common.Version.Class ( HasVersion(..) )

newtype Version = Version { version :: T.Text } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Version)

type API = Get '[JSON] Version

api :: Proxy API
api = Proxy

type ServerConstraints m a = (HasVersion a, MonadReader a m)

server :: ServerConstraints m a => ServerT API m
server = asks $ Version . getVersion
