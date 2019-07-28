module Common.Util.ServantHelpers
  ( (.:.)
  , type (++)
  , ContextType
  ) where

import Servant

type family (++) (as :: [*]) (bs :: [*]) :: [*] where
  (++) '[]    bs = bs
  (++) (a:as) bs = a : (as ++ bs)

class AppendTypeParams (t :: [*] -> *) (as :: [*]) (bs :: [*]) where
  (.:.) :: t as -> t bs -> t (as ++ bs)
infixr 3 .:.

instance AppendTypeParams Context '[] bs where
  _ .:. c = c

instance AppendTypeParams Context as bs => AppendTypeParams Context (a : as) bs where
  (a :. as) .:. bs = a :. (as .:. bs)

type family ContextType (context :: *) :: [*] where
  ContextType (Context as) = as
