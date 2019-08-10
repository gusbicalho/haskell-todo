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

import HaskellTodo.Env ( Env (..) )
import qualified HaskellTodo.HttpApi as HttpApi

type WarpAppM = ReaderT Env Handler

withSettingsFromEnv :: Env -> (Settings -> IO a) -> IO a
withSettingsFromEnv env actionFn = withStdoutLogger $ \aplogger ->
    actionFn $ setPort (#port env)
             . setLogger aplogger
             $ defaultSettings

run :: Env -> IO ()
run env = do
    let port = #port env
    putStrLn $ "Server running at port " ++ show port
    withSettingsFromEnv env $ \settings ->
      runSettings settings (HttpApi.app env provideDependencies)
  where
    provideDependencies :: WarpAppM x -> Handler x
    provideDependencies m = runReaderT m env
