module HaskellTodo.Db.Item
  ( ItemDb(..)
  ) where

import Data.Maybe (fromJust, listToMaybe)
import Data.String (fromString)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import HaskellTodo.Models.Item
import Common.Db.SQLite

class ItemDb action where
  initDB :: action ()
  getItem :: Integer -> action (Maybe Item)
  createItem :: NewItem -> action Item
  updateItem :: Item -> action (Maybe Item)
  findItemsByUserId :: Integer -> action [Item]
  deleteItem :: Integer -> action (Maybe Item)

instance ItemDb SQLiteAction where
  initDB :: SQLiteAction ()
  initDB =  SQLiteAction $ \conn ->
    execute_ conn $
      fromString $ "CREATE TABLE IF NOT EXISTS items "
                ++ "( id integer not null primary key"
                ++ ", title text not null"
                ++ ", state integer not null"
                ++ ", userId integer not null"
                ++ ", FOREIGN KEY (userId) REFERENCES users(id)"
                ++ ")"

  getItem :: Integer -> SQLiteAction (Maybe Item)
  getItem itemId = SQLiteAction $ getItem' itemId

  createItem :: NewItem -> SQLiteAction Item
  createItem newItem = SQLiteAction $ createItem' newItem

  updateItem :: Item -> SQLiteAction (Maybe Item)
  updateItem item = SQLiteAction $ updateItem' item

  findItemsByUserId :: Integer -> SQLiteAction [Item]
  findItemsByUserId userId = SQLiteAction $ findItemsByUserId' userId

  deleteItem :: Integer -> SQLiteAction (Maybe Item)
  deleteItem itemId = SQLiteAction $ deleteItem' itemId

newtype DbItemState = DbItemState { dbToItemState :: ItemState } deriving (Eq, Show)
newtype DbItem = DbItem { dbToItem :: Item } deriving (Eq, Show)
newtype DbNewItem = DbNewItem NewItem deriving (Eq, Show)

getItem' :: Integer -> Connection -> IO (Maybe Item)
getItem' rowId conn = do
  results <- query conn "SELECT id, title, state, userId FROM items WHERE id = ?" [rowId]
  let maybeDbItem = listToMaybe results
      maybeItem = dbToItem <$> maybeDbItem
  return maybeItem

createItem' :: NewItem -> Connection -> IO Item
createItem' newItem conn = do
  execute conn "INSERT INTO items (title, state, userId) values (?, ?, ?)" (DbNewItem newItem)
  rowId <- lastInsertRowId conn
  fromJust <$> getItem' (fromIntegral rowId) conn

updateItem' :: Item -> Connection -> IO (Maybe Item)
updateItem' Item { itemId, itemUserId, itemTitle, itemState } conn = do
  maybeItem <- getItem' itemId conn
  case maybeItem of
    Nothing -> return Nothing
    Just _ -> do
      executeNamed conn "UPDATE items SET title = :title, state = :state, userId = :userId WHERE id = :rowId"
                        [ ":rowId" := itemId, ":userId" := itemUserId
                        , ":title" := titleToText itemTitle, ":state" := DbItemState itemState]
      getItem' itemId conn

findItemsByUserId' :: Integer -> Connection -> IO [Item]
findItemsByUserId' userId conn = do
  results <- query conn "SELECT id, title, state, userId FROM items WHERE userId = ? ORDER BY id" [userId]
  return . map dbToItem $ results

deleteItem' :: Integer -> Connection -> IO (Maybe Item)
deleteItem' itemId conn = do
  maybeItem <- getItem' itemId conn
  case maybeItem of
    Nothing -> return ()
    Just _ -> execute conn "DELETE FROM items WHERE id = ?" [itemId]
  return maybeItem

-- Instances
instance FromField DbItemState where
  fromField sqlField = DbItemState . dbIntToState <$> fromField sqlField
    where
      dbIntToState :: Integer -> ItemState
      dbIntToState 1 = Blocked
      dbIntToState 2 = InProgress
      dbIntToState 3 = Done
      dbIntToState _ = ToDo

instance ToField DbItemState where
  toField = toField . stateToDbInt . dbToItemState
    where
      stateToDbInt :: ItemState -> Integer
      stateToDbInt Blocked    = 1
      stateToDbInt InProgress = 2
      stateToDbInt Done       = 3
      stateToDbInt ToDo       = 0

instance FromRow DbItem where
  fromRow = DbItem <$> item
    where item = Item <$> itemId <*> title <*> state <*> userId
          itemId = field
          title = textToTitle <$> field
          state = dbToItemState <$> field
          userId = field

instance ToRow DbNewItem where
  toRow (DbNewItem NewItem { newTitle            ,             newState, newUserId }) =
                     toRow ( titleToText newTitle, DbItemState newState, newUserId )
