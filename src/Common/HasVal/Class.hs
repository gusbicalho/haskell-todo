{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HasVal.Class where

import GHC.TypeLits
import GHC.OverloadedLabels

class HasVal (k :: Symbol) t env | k env -> t where
  getVal :: env -> t

instance HasVal k t env => IsLabel (k :: Symbol) (env -> t) where
  fromLabel = getVal @k
