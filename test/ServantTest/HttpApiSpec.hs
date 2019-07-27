{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Control.Monad.Reader

import ServantTest.HttpApi (app)

import ServantTest.Test.Helpers.TestEnv (testEnv, noop)

configuredApp :: IO Application
configuredApp = do
    env <- testEnv noop
    return $ app (provideDependencies env)
  where
    provideDependencies env m = runReaderT m env

spec :: Spec
spec = beforeAll configuredApp $ do
  describe "GET /ops/config" $ do
    it "responds with 200" $ do
      get "/ops/config/dump" `shouldRespondWith` 200
  describe "GET /api/version" $ do
    it "responds with 200" $ do
      get "/api/version" `shouldRespondWith` 200
