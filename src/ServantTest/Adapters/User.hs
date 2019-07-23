module ServantTest.Adapters.User where

import Prelude hiding (id)
import qualified ServantTest.Models.User as Internal
import qualified ServantTest.WireTypes.User as Wire

toWire :: Internal.User -> Wire.User
toWire Internal.User {Internal.id, Internal.name, Internal.age, Internal.email} =
           Wire.User {    Wire.id,     Wire.name,     Wire.age,     Wire.email}

singleWire :: Internal.User -> Wire.SingleUser
singleWire = Wire.SingleUser . toWire

manyWire :: [Internal.User] -> Wire.ManyUsers
manyWire = Wire.ManyUsers . map toWire

inputToNewUser :: Wire.NewUserInput -> Internal.NewUser
inputToNewUser (Wire.NewUserInput name age email) =
                Internal.NewUser  name age email
