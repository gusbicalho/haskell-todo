{-# LANGUAGE TemplateHaskell #-}

module HaskellTodo.WireTypes.Item where

import Data.Aeson.TH
import qualified Data.Text as T
import Common.Util.AesonHelpers

data ItemState = ToDo
               | Blocked
               | InProgress
               | Done
  deriving (Eq, Show, Bounded, Enum, Read)
$(deriveJSON defaultOptions ''ItemState)

data Item = Item {
  item_id :: Integer
, item_title :: T.Text
, item_state :: ItemState
, item_userId :: Integer
} deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''Item)

newtype SingleItem = SingleItem { item :: Item } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SingleItem)

newtype ManyItems = ManyItems { items :: [Item] } deriving (Eq, Show)
$(deriveJSON defaultOptions ''ManyItems)

data NewItemInput = NewItemInput {
  input_title :: T.Text
, input_state :: ItemState
, input_userId :: Integer
} deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''NewItemInput)

data ItemUpdateInput = ItemUpdateInput {
  updateInput_title :: Maybe T.Text
, updateInput_state :: Maybe ItemState
}
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''ItemUpdateInput)