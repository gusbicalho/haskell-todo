{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Redundant do" -}
module HaskellTodo.HttpApi.UserSpec (spec) where

import Test.Hspec hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Network.Wai
import Servant
import Servant.Auth.Server as SAS
import Control.Monad.Reader

import Common.Db.Transactor (Transactor(..))
import qualified HaskellTodo.Env as Env
import HaskellTodo.Auth.Types
import qualified HaskellTodo.Db.User as Db.User
import HaskellTodo.Models.User
import HaskellTodo.HttpApi.User (api, server)

import Common.Test.Helpers.Wai
import HaskellTodo.Test.Helpers.TestEnv

prepareDb :: Env.Env -> IO ()
prepareDb env = do
    let t = #transactor env
    transact t $ do
      Db.User.initDB
      Db.User.createUser user1
      Db.User.createUser user2
    return ()
  where user1 = NewUser { newLogin = "isaac@newton.com"
                        , newPassword = "qwe123"
                        }
        user2 = NewUser { newLogin = "albert@einstein.com"
                        , newPassword = "swordfish"
                        }

app :: (Env.Env -> IO (AuthResult IdentityTokenClaims)) -> IO Application
app authenticate = do
    env <- testEnv id prepareDb
    auth <- authenticate env
    let hoisted = hoistServer api (provideDependencies env) (server auth)
    return $ serve api hoisted
  where
    provideDependencies env m = runReaderT m env

spec :: Spec
spec = do
  describe "GET /:id" $ do
    before (app (const $ loginAsUser 2)) $ do
      describe "authenticated as a user" $ do
        describe "get the user" $ do
          let userRequest = get "/2"
          it "responds with 200" $ do
            userRequest `shouldRespondWith` 200
          it "responds with User" $ do
            userRequest `shouldRespondWith` [json|{user: {id: 2, login: "albert@einstein.com"}}|]
        describe "get a different user" $ do
          let userRequest = get "/99"
          it "responds with 403" $ do
            userRequest `shouldRespondWith` 403
  describe "POST /" $ do
    before (app (const $ loginAsUser 2)) $ do
      describe "as an authenticated user" $ do
        it "responds with 403" $ do
          postJson "/" [json|{login: "gust", password: "pass123"}|] `shouldRespondWith` 403
    before (app unauthenticated) $ do
      describe "unauthenticated" $ do
        it "responds with a new user" $ do
          postJson "/" [json|{login: "gust", password: "pass123"}|]
            `shouldRespondWith` [json|{user: {id: 3, login: "gust"}}|]
