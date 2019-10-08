{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module FETodo.Scratch where

import Control.Effect
import Control.Effect.Sum
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.Lift
import Control.Effect.Interpret
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Kind

data Config = Config { port :: Int
                     , version :: Text
                     }
  deriving (Eq, Show)

printConfig :: ( Member (Reader Config) sig
               , Member (Lift IO)       sig
               , Carrier sig m
               ) => m ()
printConfig = do
  config <- ask @Config
  sendM (print config)

printConfigIO :: Config -> IO ()
printConfigIO cfg = runM @IO . runReader cfg $ printConfig

data User = User {userId :: Int}
  deriving (Eq, Show, Generic)
data UserDb (m :: Type -> Type) k
  = GetUser (Maybe User -> k)
  | PutUser User k
  deriving (Generic1, Functor, HFunctor, Effect)

getUser :: ( Member UserDb sig
           , Carrier sig m
           ) => m (Maybe User)
getUser = undefined

putUser :: ( Member UserDb sig
           , Carrier sig m
           ) => User -> m ()
putUser = undefined


-- instance HFunctor UserDb where
--   fmap' = fmap
--   hmap t (GetUser i cont) = GetUser i $ \mu -> t (cont mu)

-- data UserLookupTable = ULT [(Int, User)]
-- data UserLookupTableC m a = ULTC (UserLookupTable -> m a)
--   deriving (Functor)
-- instance Monad m => Applicative (UserLookupTableC m) where
--   pure x = ULTC (const $ return x)
--   (<*>) = ap
-- instance Monad m => Monad (UserLookupTableC m) where
--   (ULTC ma) >>= f = ULTC $ \ult -> do
--     a <- ma ult
--     let (ULTC sndStep) = f a
--     sndStep ult
-- runULTC ult (ULTC f) = f ult

-- instance (Carrier sig m, Effect sig) =>
--          Carrier (UserDb :+: sig) (UserLookupTableC m)
--   where
--     eff (L (GetUser userId cont)) = cont =<< (ULTC $ \(ULT ult) ->
--       return $ lookup userId ult)
--     eff (R sig') = ULTC . const <$> eff sig'
