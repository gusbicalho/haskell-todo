module Common.Version.Class
  ( HasVersion(..)
  ) where

class HasVersion a where
  getVersion :: a -> String

instance HasVersion String where
  getVersion = id
