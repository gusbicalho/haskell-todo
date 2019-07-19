{-# LANGUAGE
    OverloadedStrings
  #-}
{- HLINT ignore "Redundant do" -}
module VersionSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Servant

import Version.Server

app :: Application
app = serve api $ hoistServer api (provideVersion "1.2.3.4") server

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "responds with version" $ do
      get "/" `shouldRespondWith` "{\"version\":\"1.2.3.4\"}"
