{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Control.Monad.Reader

import ServantTest.Config (Config(..))
import ServantTest.Env
import ServantTest.HttpApi (app)

configuredApp :: IO Application
configuredApp = do
  env <- buildEnv config
  return $ app (provideDependencies env)
  where provideDependencies env m = runReaderT m env
        config = Config {
          port = 8080
        , version = "testversion"
        , sqliteFile = ".tempdbs_usertest.db"
        }

-- configuredApp = app provideDependencies
--   where provideDependencies m = runReaderT m config

spec :: Spec
spec = with configuredApp $ do
  describe "GET /ops/version" $ do
    it "responds with 200" $ do
      get "/ops/version" `shouldRespondWith` 200
  describe "GET /ops/config" $ do
    it "responds with 200" $ do
      get "/ops/config/dump" `shouldRespondWith` 200
