{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.HasVal.Class where

import GHC.TypeLits
import GHC.OverloadedLabels

class HasVal (k :: Symbol) env t | k env -> t where
  getVal :: env -> t

instance HasVal k env t => IsLabel (k :: Symbol) (env -> t) where
  fromLabel = getVal @k
