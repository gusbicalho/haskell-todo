{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Description: Small utility server to expose the current version of a service

This Servant API simply gets a 'Version' from the monadic environment and
exposes it as JSON in an HTTP endpoint.

Notice the need to define a newtype around 'Version' to avoid an orphan
instance here. This could have been avoided if we merged "Common.Version.Types"
into this module. However, that means any module trying to provide a 'Version'
value would need to import the full server implementation.

Splitting the two modules allows, for example, for 'Version' to be moved to a
separate package with no Servant or Aeson dependencies. We could then use that
package in applications that have no HTTP API.
-}
module Common.Version.Server
  ( api
  , server
  , API
  , ServerConstraints
  , Version
  , toText
  , fromText
  ) where

import GHC.Generics
-- import Control.Monad.Reader
import Data.Proxy
import Servant
import Data.Aeson
import Common.HasField
import Common.Version.Types
import Control.Carrier.Reader

newtype WireVersion = WireVersion { version :: Version } deriving (Eq, Show, Generic)
instance ToJSON WireVersion where
  toJSON (WireVersion v) = object [("version", String $ toText v)]

type API = Get '[JSON] WireVersion

api :: Proxy API
api = Proxy

type ServerConstraints env sig m = ( Has (Reader env) sig m
                                   , HasField "version" env Version
                                   )

server :: forall env sig m . ServerConstraints env sig m => ServerT API m
server = asks @env $ WireVersion . #version
