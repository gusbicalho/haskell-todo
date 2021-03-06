module HaskellTodo.Adapters.User
  ( toWire
  , singleWire
  , manyWire
  , inputToNewUser
  ) where

import Prelude hiding (id)
import qualified HaskellTodo.Models.User as Internal
import qualified HaskellTodo.WireTypes.User as Wire

toWire :: Internal.User -> Wire.User
toWire (Internal.User id login _) =
            Wire.User id (Internal.loginToText login)

singleWire :: Internal.User -> Wire.SingleUser
singleWire = Wire.SingleUser . toWire

manyWire :: [Internal.User] -> Wire.ManyUsers
manyWire = Wire.ManyUsers . map toWire

inputToNewUser :: Wire.NewUserInput -> Internal.NewPlainUser
inputToNewUser (Wire.NewUserInput login password) =
  Internal.NewPlainUser (Internal.textToLogin login) (Internal.textToPlainPassword password)
