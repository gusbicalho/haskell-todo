{-# LANGUAGE OverloadedLabels #-}

module Common.Version.Server
  ( api
  , server
  , API
  , ServerConstraints
  , HasVal (..)
  , Version
  ) where

import GHC.Generics
import Control.Monad.Reader
import Data.Proxy
import Servant
import Data.Aeson
import Common.Version.Class ( HasVal(..), Version, toText )

newtype WireVersion = WireVersion { version :: Version } deriving (Eq, Show, Generic)
instance ToJSON WireVersion where
  toJSON (WireVersion v) = object [("version", String $ toText v)]

type API = Get '[JSON] WireVersion

api :: Proxy API
api = Proxy

type ServerConstraints m a = (HasVal "version" Version a, MonadReader a m)

server :: ServerConstraints m a => ServerT API m
server = asks $ WireVersion . #version
