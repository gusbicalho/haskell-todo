module ServantTest.Controllers.Item where

import Common.HasVal.Class
import ServantTest.Db.Transactor (Transactor(..))
import ServantTest.Models.Item
import ServantTest.Db.Item as Db.Item

type ControllerConstraints env t m stmt = (HasVal "transactor" t env, Transactor t m stmt, ItemDb stmt)

findItemsByUserId :: ControllerConstraints env t m stmt => Integer -> env -> m [Item]
findItemsByUserId userId env = do
  let transactor = getVal @"transactor" env
  transact transactor $ Db.Item.findItemsByUserId userId

getItem :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe Item)
getItem itemIdParam env = do
  let transactor = getVal @"transactor" env
  transact transactor $ Db.Item.getItem itemIdParam

createItem :: ControllerConstraints env t m stmt => NewItem -> env -> m Item
createItem newItem env = do
  let transactor = getVal @"transactor" env
  transact transactor $ Db.Item.createItem newItem
