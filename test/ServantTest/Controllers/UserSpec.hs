{- HLINT ignore "Redundant do" -}

module ServantTest.Controllers.UserSpec (spec) where

import Test.Hspec

import ServantTest.Test.Helpers.MockDb
import qualified ServantTest.Db.User as Db.User
import ServantTest.Controllers.User
import ServantTest.Models.User

spec :: Spec
spec = do
  describe "listUsers" $ do
    it "should list the users from db" $ do
      runTest (listUsers mockDb) `shouldBe` ([mockUser], [[ListUsers]])
  describe "getUser" $ do
    it "should get a user from db if it exists" $ do
      runTest (getUser 7 mockDb)
        `shouldBe` (Just mockUser, [[GetUser 7]])
    it "should return Nothing if the user does not exist" $ do
      runTest (getUser nonExistingId mockDb)
        `shouldBe` (Nothing, [[GetUser nonExistingId]])
  describe "createUser" $ do
    it "should insert the new user in the Db and return the full User" $ do
      runTest (createUser mockNewUser mockDb)
        `shouldBe` (mockUser, [[CreateUser mockNewUser]])
  describe "checkLogin" $ do
    it "should be Just a user if it is found by its login and the password matches" $ do
      runTest (checkLogin (LoginInput mockLogin mockPassword) mockDb)
        `shouldBe` (Just mockUser, [[FindUserByLogin mockLogin]])
    it "should be Nothing if the user is found by its login, but the password does not match" $ do
      runTest (checkLogin (LoginInput mockLogin "hahaha") mockDb)
        `shouldBe` (Nothing, [[FindUserByLogin mockLogin]])
    it "should be Nothing if the user is not found by its login" $ do
      runTest (checkLogin (LoginInput "notme@web.com" "hahaha") mockDb)
        `shouldBe` (Nothing, [[FindUserByLogin "notme@web.com"]])

data UserDbAction = CreateTable
                  | ListUsers
                  | CreateUser NewUser
                  | GetUser Integer
                  | FindUserByLogin Login
                  deriving (Eq, Show)

mockDb :: MockDb UserDbAction
mockDb = MockDb

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
mockPassword = "asdqwe"

mockUser :: User
mockUser = User {
  userId = 7
, userLogin = mockLogin
, userPassword = mockPassword
}

mockNewUser :: NewUser
mockNewUser = NewUser {
  newLogin = textToLogin "test@test"
, newPassword = textToPassword "asdqwe"
}
