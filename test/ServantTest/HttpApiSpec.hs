{-# LANGUAGE
    OverloadedStrings
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Control.Monad.Reader

import ServantTest.Config (Config(..))
import ServantTest.HttpApi (app)

config = Config {
  port = 8080
, version = "testversion"
}

configuredApp = app provideDependencies
  where provideDependencies m = runReaderT m config

spec :: Spec
spec = with (return configuredApp) $ do
  describe "GET /ops/version" $ do
    it "responds with 200" $ do
      get "/ops/version" `shouldRespondWith` 200
  describe "GET /ops/config" $ do
    it "responds with 200" $ do
      get "/ops/config/dump" `shouldRespondWith` 200
