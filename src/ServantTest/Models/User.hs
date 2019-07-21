module ServantTest.Models.User where

data User = User {
  id :: Integer
, name :: String
, age :: Int
, email :: String
} deriving (Eq, Show)
