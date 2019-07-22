{-# LANGUAGE DuplicateRecordFields #-}
module ServantTest.Models.User where

import qualified Data.Text as T

data User = User {
  id :: Integer
, name :: T.Text
, age :: Int
, email :: T.Text
} deriving (Eq, Show)

data NewUser = NewUser {
  newName :: T.Text
, newAge :: Int
, newEmail :: T.Text
}
