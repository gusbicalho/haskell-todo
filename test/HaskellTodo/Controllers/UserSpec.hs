{- HLINT ignore "Redundant do" -}

module HaskellTodo.Controllers.UserSpec (spec) where

import Test.Hspec

import HaskellTodo.Test.Helpers.MockEnv
import qualified HaskellTodo.Db.User as Db.User
import HaskellTodo.Controllers.User
import HaskellTodo.Models.User

spec :: Spec
spec = do
  describe "listUsers" $ do
    it "should list the users from db" $ do
      runTest (listUsers mockEnv) `shouldBe` ([mockUser], [[ListUsers]])
  describe "getUser" $ do
    it "should get a user from db if it exists" $ do
      runTest (getUser 7 mockEnv)
        `shouldBe` (Just mockUser, [[GetUser 7]])
    it "should return Nothing if the user does not exist" $ do
      runTest (getUser nonExistingId mockEnv)
        `shouldBe` (Nothing, [[GetUser nonExistingId]])
  describe "createUser" $ do
    it "should insert the new user in the Db and return the full User" $ do
      runTest (createUser mockNewPlainUser mockEnv)
        `shouldBe` (Just mockUser, [[CreateUser mockNewUser]])
  describe "checkLogin" $ do
    it "should be Just a user if it is found by its login and the password matches" $ do
      runTest (checkLogin (LoginInput mockLogin mockPlainPassword) mockEnv)
        `shouldBe` (Just mockUser, [[FindUserByLogin mockLogin]])
    it "should be Nothing if the user is found by its login, but the password does not match" $ do
      runTest (checkLogin (LoginInput mockLogin "hahaha") mockEnv)
        `shouldBe` (Nothing, [[FindUserByLogin mockLogin]])
    it "should be Nothing if the user is not found by its login" $ do
      runTest (checkLogin (LoginInput "notme@web.com" "hahaha") mockEnv)
        `shouldBe` (Nothing, [[FindUserByLogin "notme@web.com"]])

data UserDbAction = CreateTable
                  | ListUsers
                  | CreateUser NewUser
                  | GetUser Integer
                  | FindUserByLogin Login
                  deriving (Eq, Show)

mockEnv :: MockEnv UserDbAction
mockEnv = MockEnv

instance Db.User.UserDb (DbActions UserDbAction) where
  initDB = DbActions [CreateTable] ()
  listUsers = DbActions [ListUsers] [mockUser]
  getUser idParam = DbActions [GetUser idParam] $ getMockUser idParam
  createUser newUser = DbActions [CreateUser newUser] mockUser
  findUserByLogin login = DbActions [FindUserByLogin login] foundUser
    where
      foundUser | login == userLogin mockUser = Just mockUser
                | otherwise                   = Nothing

nonExistingId :: Integer
nonExistingId = 99

getMockUser :: Integer -> Maybe User
getMockUser idParam
  | idParam == nonExistingId = Nothing
  | otherwise                = Just mockUser { userId = idParam }

mockLogin :: Login
mockLogin = "test@test"

mockPassword :: Password
mockPassword = "hash_asdqwe"

mockPlainPassword :: PlainPassword
mockPlainPassword = "asdqwe"

mockUser :: User
mockUser = User {
  userId = 7
, userLogin = mockLogin
, userPassword = mockPassword
}

mockNewUser :: NewUser
mockNewUser = NewUser {
  newLogin = textToLogin "test@test"
, newPassword = mockPassword
}

mockNewPlainUser :: NewPlainUser
mockNewPlainUser = NewPlainUser {
  newPlainLogin = textToLogin "test@test"
, newPlainPassword = textToPlainPassword "asdqwe"
}
