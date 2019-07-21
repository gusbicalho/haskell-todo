module ServantTest.Models.User where

import qualified Data.Text as T

data User = User {
  id :: Integer
, name :: T.Text
, age :: Int
, email :: T.Text
} deriving (Eq, Show)
