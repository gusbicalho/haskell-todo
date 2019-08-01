{-# LANGUAGE OverloadedLabels #-}

module ServantTest.Controllers.Item where

import Common.HasVal.Class
import ServantTest.Db.Transactor (Transactor(..))
import ServantTest.Models.Item
import qualified ServantTest.Db.Item as Db.Item

type ControllerConstraints env t m stmt = (HasVal "transactor" env t, Transactor t m stmt, Db.Item.ItemDb stmt)

findItemsByUserId :: ControllerConstraints env t m stmt => Integer -> env -> m [Item]
findItemsByUserId userId env = do
  let transactor = #transactor env
  transact transactor $ Db.Item.findItemsByUserId userId

getItem :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe Item)
getItem itemIdParam env = do
  let transactor = #transactor env
  transact transactor $ Db.Item.getItem itemIdParam

getItemBelongingToUserId :: ControllerConstraints env t m stmt => Integer -> Integer -> env -> m (Maybe Item)
getItemBelongingToUserId itemIdParam userIdParam env = do
    maybeItem <- getItem itemIdParam env
    return $ maybeItem >>= guardMatchingUser
  where
    guardMatchingUser :: Item -> Maybe Item
    guardMatchingUser item@Item { itemUserId }
      | itemUserId == userIdParam = Just item
      | otherwise = Nothing

createItem :: ControllerConstraints env t m stmt => NewItem -> env -> m Item
createItem newItem env = do
  let transactor = #transactor env
  transact transactor $ Db.Item.createItem newItem
