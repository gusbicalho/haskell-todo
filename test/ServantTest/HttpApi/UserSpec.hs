{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApi.UserSpec (spec) where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai

import Network.Wai
import Servant
import Control.Monad.Reader

import qualified ServantTest.Env as Env
import Common.HasVal.Class
import ServantTest.Db.Transactor (Transactor(..))
import qualified ServantTest.Db.User as Db.User
import ServantTest.Models.User
import ServantTest.HttpApi.User (api, server)

import ServantTest.Test.Helpers.TestEnv

prepareDb :: Env.Env -> IO ()
prepareDb env = do
    let t = getVal @"transactor" env
    transact t $ do
      Db.User.initDB
      users <- Db.User.listUsers
      mapM_ (Db.User.deleteUser . userId) users
      Db.User.createUser user1
      Db.User.createUser user2
    return ()
  where user1 = NewUser { newLogin = "isaac@newton.com"
                        , newPassword = "qwe123"
                        }
        user2 = NewUser { newLogin = "albert@einstein.com"
                        , newPassword = "swordfish"
                        }

app :: IO Application
app = do
    env <- testEnv prepareDb
    let hoisted = hoistServer api (provideDependencies env) server
    return $ serve api hoisted
  where
    provideDependencies env m = runReaderT m env

spec :: Spec
spec = beforeAll app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "responds with [User]" $ do
      let users = "{\"users\":[{\"id\":1,\"login\":\"isaac@newton.com\"},{\"id\":2,\"login\":\"albert@einstein.com\"}]}"
      get "/" `shouldRespondWith` users
  describe "GET /:id" $ do
    describe "for a known id" $ do
      let userRequest = get "/2"
      it "responds with 200" $ do
        userRequest `shouldRespondWith` 200
      it "responds with User" $ do
        let user = "{\"user\":{\"id\":2,\"login\":\"albert@einstein.com\"}}"
        userRequest `shouldRespondWith` user
    describe "for an unknown id" $ do
      let userRequest = get "/99"
      it "responds with 404" $ do
        userRequest `shouldRespondWith` 404
  describe "POST /" $ do
    it "should be tested" $ do
      pendingWith "TODO"
  describe "DELETE /:id" $ do
    it "should be tested" $ do
      pendingWith "TODO"
