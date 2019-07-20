{-# LANGUAGE
    FlexibleContexts
  #-}
module ServantTest
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import Servant

import qualified Common.Config as Config
import ServantTest.HttpApi as HttpApi

startApp :: IO ()
startApp = do
  config <- Config.loadConfig
  startHttpApi config

type AppM = ReaderT Config.Config Handler

startHttpApi :: Config.Config -> IO ()
startHttpApi config = do
    let port = Config.port config
    putStrLn $ "Server running at port " ++ show port
    run port (HttpApi.app provideDependencies)
  where
    provideDependencies :: AppM x -> Handler x
    provideDependencies m = runReaderT m config
