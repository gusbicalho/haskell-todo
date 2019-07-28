module ServantTest.Test.Helpers.TestEnv where

import Control.Monad.Reader
import Data.String
import System.IO.Temp as Temp (emptySystemTempFile)
import Servant
import Servant.Auth.Server as SAS
import Common.Auth.Types as AT
import qualified ServantTest.Env as Env
import qualified ServantTest.Config as Config
import qualified ServantTest.HttpApi as HttpApi
import qualified ServantTest.HttpApi.Auth as HttpApi.Auth

tempDb :: IO FilePath
tempDb = Temp.emptySystemTempFile "test.db"

noop :: a -> IO ()
noop _ = return ()

testEnv :: (Env.Env -> Env.Env) -> (Env.Env -> IO ()) -> IO Env.Env
testEnv transEnv prepare = do
    temp <- tempDb
    env <- transEnv <$> Env.buildEnv (config temp)
    prepare env
    return env
  where
    config sqliteFile = Config.Config {
      Config.port = 8080
    , Config.version = "testversion"
    , Config.sqliteFile = sqliteFile
    , Config.jwtSecret = "a secret"
    }

testEnvApp :: (Env.Env -> Env.Env) -> (Env.Env -> IO ()) -> IO (Env.Env, Application)
testEnvApp transEnv prepare = do
    env <- testEnv transEnv prepare
    return $ (env, HttpApi.app env (provideDependencies env))
  where
    provideDependencies env m = runReaderT m env

testApp :: (Env.Env -> IO ()) -> IO Application
testApp prepare = snd <$> testEnvApp id prepare

loginAs :: String -> String -> Env.Env -> IO (AuthResult AT.AuthTokenClaims)
loginAs login password env = HttpApi.Auth.basicAuthUser env authData
  where authData = BasicAuthData (fromString login) (fromString password)

unauthenticated :: Applicative m => env -> m (AuthResult a)
unauthenticated _ = pure SAS.Indefinite
