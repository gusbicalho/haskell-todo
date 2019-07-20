{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApi.UserSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Servant

import ServantTest.HttpApi.User (api, server)

app :: Application
app = serve api server

spec :: Spec
spec = with (return app) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "responds with [User]" $ do
      let users = "[{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"},{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}]"
      get "/" `shouldRespondWith` users
    it "is able to sortBy age" $ do
      let users = "[{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"},{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}]"
      get "/?sortBy=age" `shouldRespondWith` users
    it "is able to sortBy name" $ do
      let users = "[{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"},{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"}]"
      get "/?sortBy=name" `shouldRespondWith` users
  describe "GET /:id" $ do
    describe "for a known id" $ do
      let userRequest = get "/2"
      it "responds with 200" $ do
        userRequest `shouldRespondWith` 200
      it "responds with User" $ do
        let user = "{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}"
        userRequest `shouldRespondWith` user
    describe "for an unknown id" $ do
      let userRequest = get "/99"
      it "responds with 404" $ do
        userRequest `shouldRespondWith` 404
