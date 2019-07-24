{- HLINT ignore "Redundant do" -}

module ServantTest.Controllers.UserSpec (spec) where

import Test.Hspec

import ServantTest.Test.Helpers
import qualified ServantTest.Db.User as Db.User
import ServantTest.Controllers.User
import ServantTest.Models.User

spec :: Spec
spec = do
  describe "listUsers" $ do
    it "should list the users from db" $ do
      runTest (listUsers id MockDb) `shouldBe` ([mockUser], [[DbAction "list users"]])
  describe "getUser" $ do
    it "should get a user from db if it exists" $ do
      runTest (getUser 7 MockDb)
        `shouldBe` (Just mockUser, [[DbAction "get user 7"]])
    it "should return Nothing if the user does not exist" $ do
      runTest (getUser nonExistingId MockDb)
        `shouldBe` (Nothing, [[DbAction $ "get user " ++ show nonExistingId]])
  describe "createUser" $ do
    it "should insert the new user in the Db and return the full User" $ do
      runTest (createUser mockNewUser MockDb)
        `shouldBe` (mockUser, [[DbAction $ "create user " ++ show (newName mockNewUser)]])
  describe "deleteUser" $ do
    it "should delete the existing user in the Db and return it" $ do
      runTest (deleteUser 7 MockDb)
        `shouldBe` (Just mockUser, [[DbAction $ "delete user 7"]])
    it "should attempt to delete a non-existing user, and return nothing" $ do
      runTest (deleteUser nonExistingId MockDb)
        `shouldBe` (Nothing, [[DbAction $ "delete user " ++ show nonExistingId]])

-- TODO get rid of this orphan instance somehow
instance Db.User.UserDb DbActions where
  initDB = DbActions [DbAction "create table"] ()
  listUsers = DbActions [DbAction "list users"] [mockUser]
  getUser idParam = DbActions [DbAction $ "get user " ++ show idParam] $ getMockUser idParam
  createUser newUser = DbActions [DbAction $ "create user " ++ show (newName newUser)] mockUser
  deleteUser idParam = DbActions [DbAction $ "delete user " ++ show idParam] $ getMockUser idParam

nonExistingId :: Integer
nonExistingId = 99

getMockUser :: Integer -> Maybe User
getMockUser idParam
  | idParam == nonExistingId = Nothing
  | otherwise                = Just mockUser { userId = idParam }

mockUser :: User
mockUser = User {
  userId = 7
, userName = "test"
, userAge = 25
, userEmail = "test@test"
}

mockNewUser :: NewUser
mockNewUser = NewUser {
  newName = "test"
, newAge = 25
, newEmail = "test@test"
}
