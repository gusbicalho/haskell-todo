{-# LANGUAGE
    OverloadedStrings
  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- HLINT ignore "Redundant do" -}
module Common.Config.ServerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Servant

import Common.Config.Types
import Common.Config.Server

config = Config {
  port = 8080
, version = "testversion"
}

app :: Application
app = serve api $ hoistServer api (provideConfig config) server

spec :: Spec
spec = with (return app) $ do
  describe "GET /dump" $ do
    it "responds with 200" $ do
      get "/dump" `shouldRespondWith` 200
    it "responds with version" $ do
      get "/dump" `shouldRespondWith` "{\"port\":8080,\"version\":\"testversion\"}"
