{- HLINT ignore "Redundant do" -}

module HaskellTodo.Controllers.ItemSpec (spec) where

import Test.Hspec

import Data.Functor (($>))
import HaskellTodo.Test.Helpers.MockEnv
import qualified HaskellTodo.Db.Item as Db.Item
import HaskellTodo.Controllers.Item
import HaskellTodo.Models.Item

spec :: Spec
spec = do
  describe "findItemsByUserId" $ do
    it "should query db and return items belonging to user" $
      runTest (findItemsByUserId existingItemUserId mockEnv)
      `shouldBe` ([mockItem], [[FindItemsByUserId existingItemUserId]])
  describe "getItem" $ do
    it "should query db and return and item with specified id" $
      runTest (getItem existingItemId mockEnv)
      `shouldBe` (Just mockItem, [[GetItem existingItemId]])
    it "should return Nothing if no item exist with that id" $
      runTest (getItem 123 mockEnv)
      `shouldBe` (Nothing, [[GetItem 123]])
  describe "getItemBelongingToUserId" $ do
    it "should query db and return and item with specified id, if it belongs to user" $
      runTest (getItemBelongingToUserId existingItemId existingItemUserId mockEnv)
      `shouldBe` (Just mockItem, [[GetItem existingItemId]])
    it "should return Nothing, if the Item does not belong to user" $
      runTest (getItemBelongingToUserId existingItemId 456 mockEnv)
      `shouldBe` (Nothing, [[GetItem existingItemId]])
    it "should return Nothing, if the Item does not exist" $
      runTest (getItemBelongingToUserId 123 existingItemUserId mockEnv)
      `shouldBe` (Nothing, [[GetItem 123]])
  describe "updateItem" $ do
    it "should check if item exists, if so update it in db, and return the updated item" $
      runTest (updateItem mockItemUpdate mockEnv)
      `shouldBe` (Just mockItemUpdated, [[GetItem existingItemId
                                         ,UpdateItem mockItemUpdated
                                         ]])
    it "should check if item exists, and return Nothing if it does not" $
      runTest (updateItem (mockItemUpdate { updateId = 123 }) mockEnv)
      `shouldBe` (Nothing, [[GetItem 123]])
  describe "deleteItemBelongingToUserId" $ do
    it "should check if item exists, if so delete it in db, and return the deleted item" $
      runTest (deleteItemBelongingToUserId existingItemId existingItemUserId mockEnv)
      `shouldBe` (Just mockItem, [[GetItem existingItemId
                                  ,DeleteItem existingItemId
                                  ]])
    it "should check if item exists, and return Nothing if it does not" $
      runTest (deleteItemBelongingToUserId 123 existingItemUserId mockEnv)
      `shouldBe` (Nothing, [[GetItem 123]])
  describe "createItem" $ do
    it "should insert the new Item in the Db and return the full Item" $ do
      runTest (createItem mockNewItem mockEnv)
        `shouldBe` (mockItem, [[CreateItem mockNewItem]])

data ItemDbAction = CreateTable
                  | GetItem Integer
                  | CreateItem NewItem
                  | UpdateItem Item
                  | DeleteItem Integer
                  | FindItemsByUserId Integer
                  deriving (Eq, Show)

mockEnv :: MockEnv ItemDbAction
mockEnv = MockEnv

instance Db.Item.ItemDb (DbActions ItemDbAction) where
  initDB = DbActions [CreateTable] ()
  getItem idParam = DbActions [GetItem idParam] $ getMockItem idParam
  createItem newItem = DbActions [CreateItem newItem] $ mockItem
  updateItem item = DbActions [UpdateItem item] $ getMockItem (itemId item) $> item
  findItemsByUserId userId = DbActions [FindItemsByUserId userId] [mockItem { itemUserId = userId }]
  deleteItem itemId = DbActions [DeleteItem itemId] $ getMockItem itemId

existingItemId :: Integer
existingItemId = 7

existingItemUserId :: Integer
existingItemUserId = 2

getMockItem :: Integer -> Maybe Item
getMockItem idParam
  | idParam /= existingItemId = Nothing
  | otherwise                 = Just mockItem { itemId = idParam }

mockItem :: Item
mockItem = Item {
  itemId = existingItemId
, itemTitle = "write more tests"
, itemState = ToDo
, itemUserId = existingItemUserId
}

mockNewItem :: NewItem
mockNewItem = NewItem {
  newTitle = "write more tests"
, newState = ToDo
, newUserId = existingItemUserId
}

mockItemUpdate :: ItemUpdate
mockItemUpdate = ItemUpdate {
  updateId = existingItemId
, updateUserId = existingItemUserId
, updateTitle = Just "write even more tests"
, updateState = Just InProgress
}

mockItemUpdated :: Item
mockItemUpdated = Item {
  itemId = existingItemId
, itemTitle = "write even more tests"
, itemState = InProgress
, itemUserId = existingItemUserId
}
