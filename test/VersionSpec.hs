{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
{- HLINT ignore "Redundant do" -}
module VersionSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Control.Monad.Reader
import Servant

import Version.Server (api, server, HasVersion(..))

newtype V = V String
instance HasVersion V where
  getVersion (V s) = s

app :: Application
app = serve api $ hoistServer api provideVersion server
  where provideVersion m = runReaderT m (V "1.2.3.4")

spec :: Spec
spec = with (return app) $ do
  describe "Version server" $ do
    describe "GET /" $ do
      it "responds with 200" $ do
        get "/" `shouldRespondWith` 200
      it "responds with version" $ do
        get "/" `shouldRespondWith` "{\"version\":\"1.2.3.4\"}"
