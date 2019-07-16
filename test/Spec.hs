{-# LANGUAGE
    OverloadedStrings
  #-}
{- HLINT ignore "Redundant do" -}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /users" $ do
    it "responds with 200" $ do
      get "/users" `shouldRespondWith` 200
    it "responds with [User]" $ do
      let users = "[{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"},{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}]"
      get "/users" `shouldRespondWith` users
    it "is able to sortBy age" $ do
      let users = "[{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"},{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}]"
      get "/users?sortBy=age" `shouldRespondWith` users
    it "is able to sortBy name" $ do
      let users = "[{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"},{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"}]"
      get "/users?sortBy=name" `shouldRespondWith` users
  describe "GET /user/:id" $ do
    describe "for a known id" $ do
      let userRequest = get "/user/2"
      it "responds with 200" $ do
        userRequest `shouldRespondWith` 200
      it "responds with User" $ do
        let user = "{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}"
        userRequest `shouldRespondWith` user
    describe "for an unknown id" $ do
      let userRequest = get "/user/99"
      it "responds with 404" $ do
        userRequest `shouldRespondWith` 404
