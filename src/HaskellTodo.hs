module HaskellTodo
    ( startApp
    ) where

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Control.Monad.Reader
import Servant

import qualified HaskellTodo.Config as Config
import qualified HaskellTodo.Env as Env
import qualified HaskellTodo.HttpApi as HttpApi

startApp :: IO ()
startApp = do
  config <- Config.loadConfig
  env <- Env.buildEnv config
  startHttpApi env

type ReadEnvT m = ReaderT Env.Env m

type AppM = ReadEnvT Handler

withSettingsFromEnv :: Env.Env -> (Settings -> IO a) -> IO a
withSettingsFromEnv env actionFn = withStdoutLogger $ \aplogger ->
    actionFn $ setPort (Config.port $ #config env)
             . setLogger aplogger
             $ defaultSettings

startHttpApi :: Env.Env -> IO ()
startHttpApi env = do
    let port = #port . #config $ env
    putStrLn $ "Server running at port " ++ show port
    withSettingsFromEnv env $ \settings ->
      runSettings settings (HttpApi.app env provideDependencies)
  where
    provideDependencies :: AppM x -> Handler x
    provideDependencies m = runReaderT m env
