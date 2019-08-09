module HaskellTodo (
  startApp
  ) where

import Control.Concurrent.Async ( withAsync, wait )
import HaskellTodo.Config ( loadConfig )
import HaskellTodo.Env ( buildEnv )
import qualified HaskellTodo.WarpServer as WarpServer

startApp :: IO ()
startApp = do
  config <- loadConfig
  env <- buildEnv config
  withAsync (WarpServer.run env) $ \warp ->
    wait warp
