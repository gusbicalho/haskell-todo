{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- HLINT ignore "Redundant do" -}
module Common.Config.ServerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Data.Aeson.TH
import Servant

import Common.Config.Server

data Config = Config { foo :: Int
                     , bar :: String
                     }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Config)

config = Config {
  foo = 42
, bar = "pub"
}

app :: Application
app = serve api $ hoistServer api (provideConfig config) server

spec :: Spec
spec = with (return app) $ do
  describe "GET /dump" $ do
    it "responds with 200" $ do
      get "/dump" `shouldRespondWith` 200
    it "responds with version" $ do
      get "/dump" `shouldRespondWith` "{\"foo\":42,\"bar\":\"pub\"}"
