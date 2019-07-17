{-# LANGUAGE
    FlexibleContexts
  #-}
module ServantTest.Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.Reader
import Servant

import qualified ServantTest.Config as Config
import ServantTest.HttpApi.Server (api, server)

startApp :: IO ()
startApp = do
  let config = Config.Config 8080 "0.0.1"
      port = Config.port config
  putStrLn $ "Server running at port " ++ show port
  run port (app config)

type AppM = ReaderT Config.Config Handler

provideDependencies :: Config.Config -> AppM x -> Handler x
provideDependencies config m = runReaderT m config

app :: Config.Config -> Application
app config = serve api $ hoistServer api (provideDependencies config) server
