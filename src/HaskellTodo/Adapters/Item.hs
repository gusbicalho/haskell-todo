module HaskellTodo.Adapters.Item
  ( toWire
  , manyWire
  , singleWire
  , inputToNewItem
  , inputToItemUpdate
  , toWireState
  , toInternalState
  ) where

import HaskellTodo.Models.Item as Internal
import HaskellTodo.WireTypes.Item as Wire

toWire :: Internal.Item -> Wire.Item
toWire (Internal.Item itemId title state userId) =
  Wire.Item itemId
            (Internal.titleToText title)
            (toWireState state)
            userId

manyWire :: [Internal.Item] -> Wire.ManyItems
manyWire = Wire.ManyItems . map toWire

singleWire :: Internal.Item -> Wire.SingleItem
singleWire = Wire.SingleItem . toWire

inputToNewItem :: Wire.NewItemInput -> Internal.NewItem
inputToNewItem (Wire.NewItemInput titleText wireState userId) =
  Internal.NewItem (Internal.textToTitle titleText)
                   (toInternalState wireState)
                   userId

inputToItemUpdate :: Integer -> Integer -> Wire.ItemUpdateInput -> Internal.ItemUpdate
inputToItemUpdate itemId userId (Wire.ItemUpdateInput titleText wireState) =
  Internal.ItemUpdate itemId
                      (Internal.textToTitle <$> titleText)
                      (toInternalState <$> wireState)
                      userId

toWireState :: Internal.ItemState -> Wire.ItemState
toWireState = read . show

toInternalState :: Wire.ItemState -> Internal.ItemState
toInternalState = read . show
