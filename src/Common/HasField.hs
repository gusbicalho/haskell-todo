module Common.HasField (
    HasField (..)
  , FieldAccessor ()
  , get
  , (#$), (#), (#.), (.#)
  ) where

import GHC.TypeLits
import GHC.OverloadedLabels
import GHC.Records

newtype FieldAccessor env t = FieldAccessor (env -> t)

instance HasField k env t => IsLabel (k :: Symbol) (FieldAccessor env t) where
  fromLabel = FieldAccessor $ getField @k

get :: FieldAccessor env t -> env -> t
get (FieldAccessor f) env = f env

(#$) :: FieldAccessor env t -> env -> t
accessor #$ env = get accessor env
infixr 1 #$

(#) :: FieldAccessor env t -> FieldAccessor env2 env -> FieldAccessor env2 t
FieldAccessor f # FieldAccessor g = FieldAccessor $ f . g
infixr 9 #

(#.) :: FieldAccessor env t -> (env2 -> env) -> FieldAccessor env2 t
FieldAccessor f #. g = FieldAccessor $ f . g
infixr 9 #.

(.#) :: (t -> g) -> FieldAccessor env t -> FieldAccessor env g
f .# FieldAccessor g = FieldAccessor $ f . g
infixr 9 .#

instance HasField k env t => IsLabel (k :: Symbol) (env -> t) where
  fromLabel = getField @k

