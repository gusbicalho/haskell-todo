module ServantTest.Adapters.Item where

import ServantTest.Models.Item as Internal
import ServantTest.WireTypes.Item as Wire

toWire :: Internal.Item -> Wire.Item
toWire (Internal.Item itemId title state userId) =
  Wire.Item itemId
            (Internal.titleToText title)
            (toWireState state)
            userId

toWireState :: Internal.ItemState -> Wire.ItemState
toWireState Internal.ToDo       = Wire.ToDo
toWireState Internal.Blocked    = Wire.Blocked
toWireState Internal.InProgress = Wire.InProgress
toWireState Internal.Done       = Wire.Done

manyWire :: [Internal.Item] -> Wire.ManyItems
manyWire = Wire.ManyItems . map toWire
