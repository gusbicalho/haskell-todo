module ServantTest
    ( startApp
    ) where

import Network.Wai.Handler.Warp
import Control.Monad.Reader
import Servant

import qualified ServantTest.Config as Config
import qualified ServantTest.Env as Env
import qualified ServantTest.HttpApi as HttpApi

startApp :: IO ()
startApp = do
  config <- Config.loadConfig
  env <- Env.buildEnv config
  startHttpApi env

-- type ReadConfigT m = ReaderT Config.Config m
type ReadEnvT m = ReaderT Env.Env m

type AppM = ReadEnvT Handler

startHttpApi :: Env.Env -> IO ()
startHttpApi env = do
    let port = Config.port . Env.config $ env
    putStrLn $ "Server running at port " ++ show port
    run port (HttpApi.app provideDependencies)
  where
    provideDependencies :: AppM x -> Handler x
    provideDependencies m = runReaderT m env
