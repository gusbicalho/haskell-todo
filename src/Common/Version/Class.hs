module Common.Version.Class
  ( HasVal (..)
  , Version
  , toText
  , fromText
  ) where

import qualified Data.Text as T
import Data.String (IsString(..))
import Common.HasVal.Class

newtype Version = Version { toText :: T.Text } deriving (Eq, Show)

fromText :: T.Text -> Version
fromText = Version

instance IsString Version where
  fromString = fromText . fromString

instance HasVal "version" Version Version where
  getVal = id
