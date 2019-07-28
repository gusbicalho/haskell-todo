module ServantTest.Adapters.Item where

import ServantTest.Models.Item as Internal
import ServantTest.WireTypes.Item as Wire

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

-- TODO find a better way - maybe use (read . show) + QuickChekc
toWireState :: Internal.ItemState -> Wire.ItemState
toWireState Internal.ToDo       = Wire.ToDo
toWireState Internal.Blocked    = Wire.Blocked
toWireState Internal.InProgress = Wire.InProgress
toWireState Internal.Done       = Wire.Done

toInternalState :: Wire.ItemState -> Internal.ItemState
toInternalState Wire.ToDo       = Internal.ToDo
toInternalState Wire.Blocked    = Internal.Blocked
toInternalState Wire.InProgress = Internal.InProgress
toInternalState Wire.Done       = Internal.Done
