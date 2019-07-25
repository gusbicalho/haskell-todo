module Common.Version.Class
  ( HasVal (..)
  , Version
  ) where

import qualified Data.Text as T
import Common.HasVal.Class

type Version = T.Text

instance HasVal "version" Version T.Text where
  getVal = id

instance HasVal "version" Version String where
  getVal = T.pack
