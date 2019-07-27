{- HLINT ignore "Redundant do" -}
module Common.Version.ServerSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Servant
import Control.Monad.Reader

import Common.Version.Server

app :: Application
app = serve api $ hoistServer api provideVersion server
  where provideVersion :: ReaderT Version m a -> m a
        provideVersion m = runReaderT m "1.2.3.4"

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "responds with version" $ do
      get "/" `shouldRespondWith` "{\"version\":\"1.2.3.4\"}"
