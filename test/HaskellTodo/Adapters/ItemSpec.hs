{- HLINT ignore "Redundant do" -}
module HaskellTodo.Adapters.ItemSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified HaskellTodo.Adapters.Item as A.Item
import qualified HaskellTodo.Models.Item as M.Item
import qualified HaskellTodo.WireTypes.Item as Wire.Item

spec :: Spec
spec = do
    describe "toWire" $ do
      it "should adapt the Item model to the Item wire" $
        A.Item.toWire internalItem `shouldBe` wireItem
    describe "singleWire" $ do
      it "should adapt a single internal Item to a wire SingleItem document" $
        A.Item.singleWire internalItem `shouldBe` Wire.Item.SingleItem wireItem
    describe "manyWire" $ do
      it "should adapt a list of internal Items to a wire ManyItems document" $
        A.Item.manyWire [internalItem, internalItem]
        `shouldBe` Wire.Item.ManyItems [wireItem, wireItem]
    describe "inputToNewItem" $ do
      it "should adapt a wire NewItemInput to an internal NewItem" $
        A.Item.inputToNewItem newItemInput `shouldBe` internalNewItem
    describe "inputToItemUpdate" $ do
      it "should adapt a wire ItemUpdateInput to an internal ItemUpdate" $
        A.Item.inputToItemUpdate updateItemId updateItemUserId itemUpdateInput `shouldBe` itemUpdate
    describe "toWireState" $ do
      prop "we can convert any internal ItemState to a wire ItemState" toWireStateIsComplete
    describe "toInternalState" $ do
      prop "we can convert any wire ItemState to an internal ItemState" toInternalStateIsComplete
  where
    internalItem = M.Item.Item 42 "write docs" M.Item.ToDo 3
    wireItem = Wire.Item.Item 42 "write docs" Wire.Item.ToDo 3
    newItemInput = Wire.Item.NewItemInput "write tests" Wire.Item.InProgress 7
    internalNewItem = M.Item.NewItem "write tests" M.Item.InProgress 7
    itemUpdateInput = Wire.Item.ItemUpdateInput (Just "run linter") (Just Wire.Item.InProgress)
    updateItemId = 37
    updateItemUserId = 6
    itemUpdate = M.Item.ItemUpdate updateItemId (Just "run linter") (Just M.Item.InProgress) updateItemUserId

newtype InternalItemState = IIS M.Item.ItemState deriving (Eq, Show)
instance Arbitrary InternalItemState where
  arbitrary = IIS <$> arbitraryBoundedEnum

toWireStateIsComplete :: InternalItemState -> Bool
toWireStateIsComplete (IIS itemState) = show itemState == show (A.Item.toWireState itemState)

newtype WireItemState = WIS Wire.Item.ItemState deriving (Eq, Show)
instance Arbitrary WireItemState where
  arbitrary = WIS <$> arbitraryBoundedEnum

toInternalStateIsComplete :: WireItemState -> Bool
toInternalStateIsComplete (WIS itemState) = show itemState == show (A.Item.toInternalState itemState)
