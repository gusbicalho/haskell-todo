{-# LANGUAGE AllowAmbiguousTypes #-}

module Common.HasVal.Class where

import GHC.TypeLits

class HasVal (k :: Symbol) t env | k env -> t where
  getVal :: env -> t
