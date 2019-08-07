{-|
Description: A simple Servant API to get current Config for the service.

A simple Servant API to get current Config for the service.
-}
module Common.Config.Server
  ( api
  , server
  , API
  , ServerConstraints
  , ConfigProvider
  , provideConfig
  ) where

import Control.Monad.Reader
import Data.Proxy
import Servant

type API c = "dump" :> Get '[JSON] c

api :: Proxy (API c)
api = Proxy

type ServerConstraints m c = MonadReader c m

server :: ServerConstraints m c => ServerT (API c) m
server = ask

type ConfigProvider c m a = ReaderT c m a -> m a

provideConfig :: c -> ConfigProvider c m a
provideConfig c m = runReaderT m c
