{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , TypeOperators
  #-}

module Common.Config.Server
  ( api
  , server
  , API
  , ServerConstraints
  , provideConfig
  ) where

import Control.Monad.Reader
import Data.Proxy
import Servant

import Common.Config.Types

type API = "dump" :> Get '[JSON] Config

api :: Proxy API
api = Proxy

type ServerConstraints m = MonadReader Config m

server :: ServerConstraints m => ServerT API m
server = ask

provideConfig :: Config -> ReaderT Config m a -> m a
provideConfig v m = runReaderT m v
