{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Redundant do" -}
module HaskellTodo.HttpApi.ItemSpec (spec) where

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
import qualified HaskellTodo.Db.Item as Db.Item
import HaskellTodo.Models.Item
import HaskellTodo.HttpApi.Item (api, server)

import Common.Test.Helpers.Wai
import HaskellTodo.Test.Helpers.TestEnv

prepareDb :: Env.Env -> IO ()
prepareDb env = do
    let t = #transactor env
    transact t $ do
      Db.Item.initDB
      Db.Item.createItem item1
      Db.Item.createItem item2
    return ()
  where item1 = NewItem { newTitle = "do a barrel roll"
                        , newState = ToDo
                        , newUserId = 1
                        }
        item2 = NewItem { newTitle = "write a book"
                        , newState = Blocked
                        , newUserId = 2
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
  describe "authenticated as a user" $ do
    beforeAll (app (const $ loginAsUser 2)) $ do
      describe "GET /:id" $ do
        describe "get the item that belongs to user" $ do
          it "responds with Item" $ do
            get "/2" `shouldRespondWith` [json|{item: {id: 2
                                                      ,title: "write a book"
                                                      ,state: "Blocked"
                                                      ,userId: 2}}|]
        describe "get item that belongs to a different user" $ do
          it "responds with 404" $ do
            get "/1" `shouldRespondWith` 404
    beforeAll (app (const $ loginAsUser 2)) $ do
      describe "PUT /:id" $ do
        describe "update item that belongs to user" $ do
          it "responds with updated item" $ do
            putJson "/2" [json|{title: "write a great book", state: "InProgress"}|]
              `shouldRespondWith` [json|{item: {id: 2
                                              ,title: "write a great book"
                                              ,state: "InProgress"
                                              ,userId: 2}}|]
          it "after updating, GET returns the updated item" $ do
            get "/2" `shouldRespondWith` [json|{item: {id: 2
                                                      ,title: "write a great book"
                                                      ,state: "InProgress"
                                                      ,userId: 2}}|]
        describe "update item that belongs to other user" $ do
          it "responds with 404" $ do
            putJson "/1" [json|{title: "write a great book", state: "InProgress"}|]
              `shouldRespondWith` 404
    beforeAll (app (const $ loginAsUser 2)) $ do
      describe "DELETE /:id" $ do
        describe "delete item that belongs to user" $ do
          it "responds with the deleted item" $ do
            delete "/2"
              `shouldRespondWith` [json|{item: {id: 2
                                              ,title: "write a book"
                                              ,state: "Blocked"
                                              ,userId: 2}}|]
          it "after deleting, GET returns 404" $ do
            get "/2" `shouldRespondWith` 404
        describe "delete item that belongs to other user" $ do
          it "responds with 404" $ do
            delete "/1" `shouldRespondWith` 404
