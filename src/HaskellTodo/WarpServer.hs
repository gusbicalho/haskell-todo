{-|
Description: Wires up the Warp server for out HTTP API
-}
module HaskellTodo.WarpServer (
    run
  ) where

import Network.Wai.Handler.Warp hiding ( run )
import Network.Wai.Logger

import Control.Carrier.Reader (runReader)
import Control.Carrier.Lift (runM)
import Control.Carrier.Error.Either (runError)
import Control.Monad.Except
import Common.Effect.JWT
import Servant

import HaskellTodo.Env ( Env (..) )
import qualified HaskellTodo.HttpApi as HttpApi

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
      runSettings settings (HttpApi.app env ( (>>= \case
                                                     Left err -> throwError err
                                                     Right v -> return v)
                                            . runM
                                            . runJWTOps
                                            . runError @ServerError
                                            . runReader env
                                            ))
