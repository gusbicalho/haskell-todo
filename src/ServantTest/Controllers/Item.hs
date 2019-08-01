{-# LANGUAGE OverloadedLabels #-}

module ServantTest.Controllers.Item where

import Common.HasVal.Class
import ServantTest.Db.Transactor (Transactor(..))
import ServantTest.Models.Item
import qualified ServantTest.Db.Item as Db.Item

type ControllerConstraints env t m action = (HasVal "transactor" env t, Transactor t m action, Db.Item.ItemDb action)

findItemsByUserId :: ControllerConstraints env t m action => Integer -> env -> m [Item]
findItemsByUserId userId env = do
  let transactor = #transactor env
  transact transactor $ Db.Item.findItemsByUserId userId

getItem :: ControllerConstraints env t m action => Integer -> env -> m (Maybe Item)
getItem itemIdParam env = do
  let transactor = #transactor env
  transact transactor $ Db.Item.getItem itemIdParam

getItemBelongingToUserId :: ControllerConstraints env t m action => Integer -> Integer -> env -> m (Maybe Item)
getItemBelongingToUserId itemIdParam userIdParam env = do
    maybeItem <- getItem itemIdParam env
    return $ maybeItem >>= guardMatchingUser userIdParam

updateItem :: ControllerConstraints env t m action => ItemUpdate -> env -> m (Maybe Item)
updateItem (ItemUpdate { updateId, updateUserId, updateTitle, updateState }) env = do
    transact (#transactor env) $ do
      maybeItem <- Db.Item.getItem updateId
      case guardMatchingUser updateUserId =<< maybeItem of
        Nothing -> return Nothing
        Just item -> do
          Db.Item.updateItem item { itemTitle = updateTitle
                                  , itemState = updateState
                                  }

deleteItemBelongingToUserId :: ControllerConstraints env t m action => Integer -> Integer -> env -> m (Maybe Item)
deleteItemBelongingToUserId itemIdParam userIdParam env = do
    transact (#transactor env) $ do
      maybeItem <- Db.Item.getItem itemIdParam
      case guardMatchingUser userIdParam =<< maybeItem of
        Nothing -> return Nothing
        Just item -> Db.Item.deleteItem (itemId item)

guardMatchingUser :: Integer -> Item -> Maybe Item
guardMatchingUser userIdParam item@Item { itemUserId }
  | itemUserId == userIdParam = Just item
  | otherwise = Nothing

createItem :: ControllerConstraints env t m action => NewItem -> env -> m Item
createItem newItem env = do
  let transactor = #transactor env
  transact transactor $ Db.Item.createItem newItem
