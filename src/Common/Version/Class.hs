module Common.Version.Class
  ( HasVersion(..)
  ) where

import qualified Data.Text as T

class HasVersion a where
  getVersion :: a -> T.Text

instance HasVersion String where
  getVersion = T.pack

instance HasVersion T.Text where
  getVersion = id
