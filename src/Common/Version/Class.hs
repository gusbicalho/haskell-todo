module Common.Version.Class
  ( HasVal (..)
  , Version
  , toText
  , fromText
  ) where

import qualified Data.Text as T
import Common.HasVal.Class

newtype Version = Version { toText :: T.Text } deriving (Eq, Show)

fromText :: T.Text -> Version
fromText = Version

instance HasVal "version" Version T.Text where
  getVal = Version

instance HasVal "version" Version String where
  getVal = Version . T.pack

instance HasVal "version" Version Version where
  getVal = id
