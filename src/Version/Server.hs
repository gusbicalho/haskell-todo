{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , OverloadedStrings
  , TemplateHaskell
  , TypeOperators
  #-}

module Version.Server
  ( api
  , server
  , API
  , ServerConstraints
  , HasVersion(..)
  , provideVersion
  ) where

import Control.Monad.Reader
import Data.Proxy
import Servant
import Data.Aeson.TH
import Version.Class ( HasVersion(..) )

newtype Version = Version { version :: String } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Version)

type API = Get '[JSON] Version

api :: Proxy API
api = Proxy

type ServerConstraints m a = (HasVersion a, MonadReader a m)

server :: ServerConstraints m a => ServerT API m
server = asks $ Version . getVersion

provideVersion :: String -> ReaderT String m a -> m a
provideVersion v m = runReaderT m v
