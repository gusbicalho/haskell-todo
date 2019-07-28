module ServantTest.Models.Item
  ( Item (..)
  , ItemState (..)
  , NewItem (..)
  , Title
  , titleToText
  , textToTitle
  ) where

import Data.String
import qualified Data.Text as T

data ItemState = ToDo
               | Blocked
               | InProgress
               | Done
  deriving (Eq, Show)

newtype Title = Title { titleToText :: T.Text } deriving (Eq, Show)

textToTitle :: T.Text -> Title
textToTitle = Title

instance IsString Title where
  fromString = Title . T.pack

data Item = Item {
  itemId :: Integer
, itemTitle :: Title
, itemState :: ItemState
, itemUserId :: Integer
} deriving (Eq, Show)

data NewItem = NewItem {
  newTitle :: Title
, newState :: ItemState
, newUserId :: Integer
} deriving (Eq, Show)
