module ServantTest.Adapters.User where

import Prelude hiding (id)
import qualified ServantTest.Models.User as Internal
import qualified ServantTest.WireTypes.User as Wire

toWire :: Internal.User -> Wire.User
toWire (Internal.User id login _) =
            Wire.User id (Internal.loginToText login)

singleWire :: Internal.User -> Wire.SingleUser
singleWire = Wire.SingleUser . toWire

manyWire :: [Internal.User] -> Wire.ManyUsers
manyWire = Wire.ManyUsers . map toWire

inputToNewUser :: Wire.NewUserInput -> Internal.NewUser
inputToNewUser (Wire.NewUserInput login password) =
                Internal.NewUser (Internal.textToLogin login) (Internal.textToPassword password)
