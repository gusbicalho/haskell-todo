{-# LANGUAGE RecordWildCards #-}
module ServantTest.Adapters.User where

import Prelude hiding (id)
import qualified ServantTest.Models.User as Internal
import qualified ServantTest.WireTypes.User as Wire

toWire :: Internal.User -> Wire.User
toWire Internal.User {..} = Wire.User {..}

singleWire :: Internal.User -> Wire.SingleUser
singleWire = Wire.SingleUser . toWire

manyWire :: [Internal.User] -> Wire.ManyUsers
manyWire = Wire.ManyUsers . map toWire
