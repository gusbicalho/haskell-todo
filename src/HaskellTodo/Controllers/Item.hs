{-# LANGUAGE OverloadedLabels #-}

module HaskellTodo.Controllers.Item where

import Common.HasField
import Common.Db.Transactor (Transactor(..))
import HaskellTodo.Models.Item
import qualified HaskellTodo.Db.Item as Db.Item

type ControllerConstraints env t m action = ( HasField "transactor" env t
                                            , Transactor t m action
                                            , Db.Item.ItemDb action
                                            , Monad action
                                            )

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
updateItem ItemUpdate { updateId, updateUserId, updateTitle, updateState } env =
    transact (#transactor env) $ do
      maybeItem <- Db.Item.getItem updateId
      case guardMatchingUser updateUserId =<< maybeItem of
        Nothing   -> return Nothing
        Just item -> Db.Item.updateItem $ updatingTitle updateTitle
                                        . updatingState updateState
                                        $ item
  where updatingTitle Nothing         item = item
        updatingTitle (Just newTitle) item = item { itemTitle = newTitle }
        updatingState Nothing         item = item
        updatingState (Just newState) item = item { itemState = newState }

deleteItemBelongingToUserId :: ControllerConstraints env t m action => Integer -> Integer -> env -> m (Maybe Item)
deleteItemBelongingToUserId itemIdParam userIdParam env =
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
