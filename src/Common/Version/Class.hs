module Common.Version.Class
  ( HasField (..)
  , Version
  , toText
  , fromText
  ) where

import qualified Data.Text as T
import Data.String (IsString(..))
import Common.HasField

newtype Version = Version { toText :: T.Text } deriving (Eq, Show)

fromText :: T.Text -> Version
fromText = Version

instance IsString Version where
  fromString = fromText . fromString

instance HasField "version" Version Version where
  getField = id
