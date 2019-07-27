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
  describe "deleteUser" $ do
    it "should delete the existing user in the Db and return it" $ do
      runTest (deleteUser 7 mockDb)
        `shouldBe` (Just mockUser, [[DeleteUser 7]])
    it "should attempt to delete a non-existing user, and return nothing" $ do
      runTest (deleteUser nonExistingId mockDb)
        `shouldBe` (Nothing, [[DeleteUser nonExistingId]])

data UserDbAction = CreateTable
                  | ListUsers
                  | CreateUser NewUser
                  | GetUser Integer
                  | DeleteUser Integer
                  deriving (Eq, Show)

mockDb :: MockDb UserDbAction
mockDb = MockDb

instance Db.User.UserDb (DbActions UserDbAction) where
  initDB = DbActions [CreateTable] ()
  listUsers = DbActions [ListUsers] [mockUser]
  getUser idParam = DbActions [GetUser idParam] $ getMockUser idParam
  createUser newUser = DbActions [CreateUser newUser] mockUser
  deleteUser idParam = DbActions [DeleteUser idParam] $ getMockUser idParam

nonExistingId :: Integer
nonExistingId = 99

getMockUser :: Integer -> Maybe User
getMockUser idParam
  | idParam == nonExistingId = Nothing
  | otherwise                = Just mockUser { userId = idParam }

mockUser :: User
mockUser = User {
  userId = 7
, userLogin = textToLogin "test@test"
, userPassword = textToPassword "asdqwe"
}

mockNewUser :: NewUser
mockNewUser = NewUser {
  newLogin = textToLogin "test@test"
, newPassword = textToPassword "asdqwe"
}
