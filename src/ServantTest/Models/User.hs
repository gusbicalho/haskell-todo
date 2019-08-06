module ServantTest.Models.User
  ( User (..)
  , NewUser (..)
  , NewPlainUser (..)
  , Login
  , textToLogin
  , loginToText
  , Password
  , textToPassword
  , passwordToText
  , PlainPassword
  , plainPasswordToText
  , textToPlainPassword
  , LoginInput (..)
  ) where

import Data.String
import qualified Data.Text as T

newtype Login = Login { loginToText :: T.Text } deriving (Eq, Show)

textToLogin :: T.Text -> Login
textToLogin = Login

instance IsString Login where
  fromString = Login . T.pack

newtype Password = Password { passwordToText :: T.Text } deriving (Eq, Show)
textToPassword :: T.Text -> Password
textToPassword = Password
instance IsString Password where
  fromString = Password . T.pack

newtype PlainPassword = PlainPassword { plainPasswordToText :: T.Text } deriving (Eq, Show)
textToPlainPassword :: T.Text -> PlainPassword
textToPlainPassword = PlainPassword
instance IsString PlainPassword where
  fromString = PlainPassword . T.pack

data User = User {
  userId :: Integer
, userLogin :: Login
, userPassword :: Password
} deriving (Eq, Show)

data NewPlainUser = NewPlainUser {
  newPlainLogin :: Login
, newPlainPassword :: PlainPassword
} deriving (Eq, Show)

data NewUser = NewUser {
  newLogin :: Login
, newPassword :: Password
} deriving (Eq, Show)

data LoginInput = LoginInput {
  loginInputLogin :: Login
, loginInputPassword :: PlainPassword
} deriving (Eq, Show)
