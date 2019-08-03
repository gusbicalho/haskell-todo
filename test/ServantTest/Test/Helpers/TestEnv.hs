module ServantTest.Test.Helpers.TestEnv where

import Control.Monad.Reader
import System.IO.Temp as Temp (emptySystemTempFile)
import Servant
import Servant.Auth.Server as SAS
import ServantTest.Auth.WireTypes
import qualified ServantTest.Env as Env
import qualified ServantTest.Config as Config
import qualified ServantTest.HttpApi as HttpApi

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
    , Config.jwtKeyPath = "resources/test.key"
    , Config.insecureAuthCookie = True
    }

testEnvApp :: (Env.Env -> Env.Env) -> (Env.Env -> IO ()) -> IO (Env.Env, Application)
testEnvApp transEnv prepare = do
    env <- testEnv transEnv prepare
    return (env, HttpApi.app env (provideDependencies env))
  where
    provideDependencies env m = runReaderT m env

testApp :: (Env.Env -> IO ()) -> IO Application
testApp prepare = snd <$> testEnvApp id prepare

loginAsUser :: Integer -> IO (AuthResult IdentityTokenClaims)
loginAsUser userId = return $ SAS.Authenticated claims
  where
    claims = AuthTokenClaims {
      identity = Known $ User {
        userId = userId
      }
    }

unauthenticated :: Applicative m => env -> m (AuthResult a)
unauthenticated _ = pure SAS.Indefinite
