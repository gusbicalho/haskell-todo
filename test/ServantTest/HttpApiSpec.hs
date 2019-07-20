{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Servant
import Control.Monad.Reader

import Common.Config (Config(..))
import ServantTest.HttpApi (app)

configuredApp = app provideDependencies
  where provideDependencies m = runReaderT m ("1.2.3" :: String)

spec :: Spec
spec = with (return configuredApp) $ do
  describe "GET /api/version" $ do
    it "responds with 200" $ do
      get "/api/version" `shouldRespondWith` 200
