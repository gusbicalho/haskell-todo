{- HLINT ignore "Redundant do" -}

module ServantTest.Controllers.AuthSpec (spec) where

import Test.Hspec

import ServantTest.Test.Helpers.MockDb
import qualified ServantTest.Db.User as Db.User
import ServantTest.Controllers.Auth
import ServantTest.Models.User

spec :: Spec
spec = do
  describe "checkUserLogin" $ do
    it "should be Just a user if it is found by its login and the password matches" $ do
      runTest (checkUserLogin (LoginInput correctLogin correctPassword) mockDb)
        `shouldBe` (Just existingUser, [[FindUserByLogin correctLogin]])
    it "should be Nothing if the user is found by its login, but the password does not match" $ do
      runTest (checkUserLogin (LoginInput correctLogin wrongPassword) mockDb)
        `shouldBe` (Nothing, [[FindUserByLogin correctLogin]])
    it "should be Nothing if the user is not found by its login" $ do
      runTest (checkUserLogin (LoginInput wrongLogin wrongPassword) mockDb)
        `shouldBe` (Nothing, [[FindUserByLogin wrongLogin]])

correctLogin :: Login
correctLogin = "qwe"
correctPassword :: Password
correctPassword = "qweqwe"
wrongLogin :: Login
wrongLogin = "asd"
wrongPassword :: Password
wrongPassword = "asdasd"

existingUser :: User
existingUser = User {
  userId = 7
, userLogin = correctLogin
, userPassword = correctPassword
}

data UserDbAction = FindUserByLogin Login deriving (Eq, Show)

mockDb :: MockDb UserDbAction
mockDb = MockDb

instance Db.User.UserDb (DbActions UserDbAction) where
  findUserByLogin login = DbActions [FindUserByLogin login] $ foundUser
    where
      foundUser | login == (userLogin existingUser) = Just existingUser
                | otherwise = Nothing
  initDB = undefined
  listUsers = undefined
  getUser = undefined
  createUser = undefined
