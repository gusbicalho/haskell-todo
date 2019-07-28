{-# LANGUAGE TemplateHaskell #-}

module ServantTest.WireTypes.User where

import Servant
import Data.Aeson.TH
import qualified Data.Text as T
import Common.Util.AesonHelpers

data User = User {
  user_id :: Integer
, user_login :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''User)

newtype SingleUser = SingleUser { user :: User } deriving (Eq, Show)
$(deriveJSON defaultOptions ''SingleUser)

newtype ManyUsers = ManyUsers { users :: [User] } deriving (Eq, Show)
$(deriveJSON defaultOptions ''ManyUsers)

data NewUserInput = NewUserInput {
  input_login :: T.Text
, input_password :: T.Text
} deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''NewUserInput)
