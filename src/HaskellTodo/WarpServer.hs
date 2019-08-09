{-|
Description: Wires up the Warp server for out HTTP API
-}
module HaskellTodo.WarpServer (
    run
  ) where

import Network.Wai.Handler.Warp hiding ( run )
import Network.Wai.Logger
import Control.Monad.Reader
import Servant

import qualified HaskellTodo.Config as Config
import qualified HaskellTodo.Env as Env
import qualified HaskellTodo.HttpApi as HttpApi

type WarpAppM = ReaderT Env.Env Handler

withSettingsFromEnv :: Env.Env -> (Settings -> IO a) -> IO a
withSettingsFromEnv env actionFn = withStdoutLogger $ \aplogger ->
    actionFn $ setPort (Config.port $ #config env)
             . setLogger aplogger
             $ defaultSettings

run :: Env.Env -> IO ()
run env = do
    let port = #port . #config $ env
    putStrLn $ "Server running at port " ++ show port
    withSettingsFromEnv env $ \settings ->
      runSettings settings (HttpApi.app env provideDependencies)
  where
    provideDependencies :: WarpAppM x -> Handler x
    provideDependencies m = runReaderT m env
