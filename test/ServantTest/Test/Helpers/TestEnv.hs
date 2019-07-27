module ServantTest.Test.Helpers.TestEnv where

import System.IO.Temp as Temp (emptySystemTempFile)
import qualified ServantTest.Env as Env
import qualified ServantTest.Config as Config

tempDb :: IO FilePath
tempDb = Temp.emptySystemTempFile "test.db"

noop :: a -> IO ()
noop _ = return ()

testEnv :: (Env.Env -> IO ()) -> IO Env.Env
testEnv prepare = do
    temp <- tempDb
    env <- Env.buildEnv (config temp)
    prepare env
    return env
  where
    config sqliteFile = Config.Config {
      Config.port = 8080
    , Config.version = "testversion"
    , Config.sqliteFile = sqliteFile
    }
