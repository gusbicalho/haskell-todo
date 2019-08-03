{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import ServantTest.Test.Helpers.TestEnv (testApp, noop)

spec :: Spec
spec = beforeAll (testApp noop) $ do
  describe "GET /ops/config" $ do
    it "responds with 200" $ do
      get "/ops/config/dump" `shouldRespondWith` 200
  describe "GET /api/version" $ do
    it "responds with 200" $ do
      get "/api/version" `shouldRespondWith` 200
