{- HLINT ignore "Redundant do" -}
module ServantTest.HttpApi.UserSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai
import Servant
import Control.Monad.Reader

import qualified ServantTest.Env as Env
import qualified ServantTest.Config as Config
import ServantTest.Db.Transactor (Transactor(..), HasTransactor(..))
import qualified ServantTest.Db.User as Db.User
import ServantTest.Models.User
import ServantTest.HttpApi.User (api, server)

dbfile :: FilePath
dbfile = ".tempdbs_usertest.db"

prepareDb :: Env.Env -> IO ()
prepareDb env = do
    let t = getTransactor env
    transact t $ do
      users <- Db.User.listUsers
      mapM_ (Db.User.deleteUser . userId) users
      Db.User.createUser user1
      Db.User.createUser user2
    return ()
  where user1 = NewUser { newName = "Isaac Newton"
                        , newAge = 26
                        , newEmail = "isaac@newton.com"
                        }
        user2 = NewUser { newName = "Albert Einstein"
                        , newAge = 42
                        , newEmail = "albert@einstein.com"
                        }

app :: IO Application
app = do
  env <- Env.buildEnv config
  prepareDb env
  let hoisted = hoistServer api (provideDependencies env) server
  return $ serve api hoisted
  where provideDependencies env m = runReaderT m env
        config = Config.Config {
          Config.port = 8080
        , Config.version = "testversion"
        , Config.sqliteFile = dbfile
        }

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "responds with [User]" $ do
      let users = "{\"users\":[{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"},{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}]}"
      get "/" `shouldRespondWith` users
    it "is able to sortBy age" $ do
      let users = "{\"users\":[{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"},{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}]}"
      get "/?sortBy=age" `shouldRespondWith` users
    it "is able to sortBy name" $ do
      let users = "{\"users\":[{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"},{\"id\":1,\"name\":\"Isaac Newton\",\"age\":26,\"email\":\"isaac@newton.com\"}]}"
      get "/?sortBy=name" `shouldRespondWith` users
  describe "GET /:id" $ do
    describe "for a known id" $ do
      let userRequest = get "/2"
      it "responds with 200" $ do
        userRequest `shouldRespondWith` 200
      it "responds with User" $ do
        let user = "{\"user\":{\"id\":2,\"name\":\"Albert Einstein\",\"age\":42,\"email\":\"albert@einstein.com\"}}"
        userRequest `shouldRespondWith` user
    describe "for an unknown id" $ do
      let userRequest = get "/99"
      it "responds with 404" $ do
        userRequest `shouldRespondWith` 404
