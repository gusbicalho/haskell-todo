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
import GHC.Generics (Generic, Generic1)
import Data.Kind
import Data.Maybe (listToMaybe)

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

-- UserDB effect
data User = User {userId :: Int, userName :: Text}
  deriving (Eq, Show, Generic)
data UserDb m k
  = GetUser Int (Maybe User -> m k)
  | PutUser User (m k)
  deriving (Generic1, Functor, HFunctor, Effect)

getUser :: ( Member UserDb sig
           , Carrier sig m
           ) => Int -> m (Maybe User)
getUser userId = send (GetUser userId pure)

putUser :: ( Member UserDb sig
           , Carrier sig m
           ) => User -> m ()
putUser user = send (PutUser user (pure ()))

-- usage
renameUser :: ( Member UserDb sig
              , Carrier sig m
              ) => Int -> (Text -> Text) -> m (Maybe User)
renameUser userId rename = do
  maybeUser <- getUser userId
  case maybeUser of
    Nothing -> return Nothing
    Just user ->
      let user' = user { userName = rename (userName user) }
      in do putUser user'
            getUser userId

-- handler

newtype UserList = UserList [User]
  deriving (Eq, Show)

newtype UserListDb m a = UserListDb { runUserListDb :: UserList -> m (UserList, a) }

execUserListDb :: Functor m => UserList -> UserListDb m a -> m UserList
execUserListDb ul uldb = fst <$> runUserListDb uldb ul

evalUserListDb :: Functor m => UserList -> UserListDb m a -> m a
evalUserListDb ul uldb = snd <$> runUserListDb uldb ul

instance (Monad m) => Functor (UserListDb m) where
  fmap = liftM

instance (Monad m) => Applicative (UserListDb m) where
  pure a = UserListDb $ \u -> return (u, a)
  (<*>) = ap

instance (Monad m) => Monad (UserListDb m) where
  return = pure
  (UserListDb runDb) >>= f = UserListDb $ \ul -> do
    (ul', a) <- runDb ul
    let (UserListDb runDb2) = f a
    runDb2 ul'

instance (Carrier sig m, Effect sig) => Carrier (UserDb :+: sig) (UserListDb m) where
  eff (L (GetUser uid  k)) = UserListDb $ \ul -> runUserListDb (k $ findUserInList uid ul) ul
  eff (L (PutUser user k)) = UserListDb $ \ul -> runUserListDb k (putUserInList user ul)
  eff (R other) = UserListDb $ \ul -> eff (handle (ul, ()) (uncurry $ flip runUserListDb) other)

findUserInList :: Int -> UserList -> Maybe User
findUserInList userId (UserList us) = listToMaybe . filter idMatch $ us
  where idMatch User { userId = userId' } = userId == userId'
putUserInList :: User -> UserList -> UserList
putUserInList user (UserList ulist) = UserList $ go ulist
  where go [] = [user]
        go (u:us) | (userId u == userId user) = user : us
                  | otherwise = u : go us

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
