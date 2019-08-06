{- HLINT ignore "Redundant do" -}
module ServantTest.Adapters.UserSpec (spec) where

import Test.Hspec
import qualified ServantTest.Adapters.User as A.User
import qualified ServantTest.Models.User as M.User
import qualified ServantTest.WireTypes.User as Wire.User

spec :: Spec
spec = do
    describe "toWire" $ do
      it "should adapt the User model to the User wire" $
        A.User.toWire internalUser `shouldBe` wireUser
    describe "singleWire" $ do
      it "should adapt a single internal User to a wire SingleUser document" $
        A.User.singleWire internalUser `shouldBe` Wire.User.SingleUser wireUser
    describe "manyWire" $ do
      it "should adapt a list of internal Users to a wire ManyUsers document" $
        A.User.manyWire [internalUser, internalUser]
        `shouldBe` Wire.User.ManyUsers [wireUser, wireUser]
    describe "inputToNewUser" $ do
      it "should adapt a wire NewUserInput to an internal NewUser" $
        A.User.inputToNewUser newUserInput `shouldBe` internalNewUser
  where
    internalUser = M.User.User 42 "foo" "bar"
    wireUser = Wire.User.User 42 "foo"
    newUserInput = Wire.User.NewUserInput "foo" "bar"
    internalNewUser = M.User.NewPlainUser "foo" "bar"
