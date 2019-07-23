{-# LANGUAGE DuplicateRecordFields #-}
module ServantTest.Models.User where

import qualified Data.Text as T

data User = User {
  userId :: Integer
, userName :: T.Text
, userAge :: Int
, userEmail :: T.Text
} deriving (Eq, Show)

data NewUser = NewUser {
  newName :: T.Text
, newAge :: Int
, newEmail :: T.Text
}
