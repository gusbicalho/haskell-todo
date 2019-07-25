{-# LANGUAGE TemplateHaskell #-}

module Common.Version.Server
  ( api
  , server
  , API
  , ServerConstraints
  , HasVal (..)
  , Version
  ) where

import Control.Monad.Reader
import Data.Proxy
import Servant
import Data.Aeson.TH
import Common.Version.Class ( HasVal(..), Version )

newtype WireVersion = WireVersion { version :: Version } deriving (Eq, Show)
$(deriveJSON defaultOptions ''WireVersion)

type API = Get '[JSON] WireVersion

api :: Proxy API
api = Proxy

type ServerConstraints m a = (HasVal "version" Version a, MonadReader a m)

server :: ServerConstraints m a => ServerT API m
server = asks $ WireVersion . getVal @"version"
