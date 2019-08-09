{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Description: Convenience module around GHC.Records and GHC.OverloadedLabels

This module re-exports the @HasField@ typeclass from @GHC.Records@. It also
defines an orphan instance for the @IsLabel@ typeclass, which allows us to
use the @OverloadedLabels@ extension for nicer syntax, instead of calling
@getField@ directly (see example below).

This combination of @HasField@ and @OverloadedLabels@ is used throughout this
codebase to allow a flexible kind of "dependency injection". Application code
can define a single @Env@ type that provides the necessary fields for all
Common functionality, as well as all application code. On the other hand, we
can keep most of our code decoupled from this @Env@ type by declaring
dependencies as 'HasField' constraints.

Example:

@
data Env = Env {
  foo :: String
, bar :: Integer
}

-- HasField by itself allows us to define the fn below
-- Notice the usage of TypeApplications to pass a compile-time symbol to the
-- HasField typeclass
showFoo :: (HasField "foo" t foo, Show foo) => t -> String
showFoo = show . getField @"foo"

myFoo = showFoo $ Env "fooz" 27

-- With our orphan instance and the OverloadedLabels extension, we can compile
-- the prettier version below:
showFoo2 :: (HasField "foo" t foo, Show foo) => t -> String
showFoo2 = show . #foo

myFoo2 = showFoo2 $ Env "fooz" 27
@

Regarding the choice to add an orphan instance: this is an opinionated decision
to instantiate @IsLabel@ for @(->)@ using the simplest possible approach. Other
options exist - for example, one could define an instance of @IsLabel@ that
returns some kind of lens, like this module does:
<https://hackage.haskell.org/package/generic-lens-labels>.
We choose the direct approach of using labels as simple getter functions. Code
that wishes to follow this decision should never import GHC.Record or
GHC.OverloadedLabels directly. Instead, use this module instead to ensure we
never forget to get the correct orphan instance.

I toyed with a small DSL around a newtype @FieldAccessor@ to avoid the orphan
instance. I found that the result was a bit ugly, for effectively no gain. If
we were use a DSL for accessing field, I would probably be better to just go
with lenses. Anyway, you can see what it could have looked like:

@
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

-- example usage
data Config = Config {
  version :: String
, port :: Int
}

data Env = Env {
  foobs :: String
, config :: Config
}

-- using get to apply the FieldAccessor
envPort :: Env -> Int
envPort = get $ #port # #config

-- using #$ instead of get
envVersion :: Env -> String
envVersion env = #version # #config #$ env

-- compose a FieldAccessor with a normal function
envTextVersion :: Env -> Text
envTextVersion = get $ pack .# #version # #config
@
-}
module Common.HasField (
    HasField (..)
  ) where

import GHC.TypeLits
import GHC.OverloadedLabels
import GHC.Records

instance HasField k env t => IsLabel (k :: Symbol) (env -> t) where
  fromLabel = getField @k
