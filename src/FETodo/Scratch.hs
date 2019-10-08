{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DerivingStrategies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module FETodo.Scratch where

import Control.Effect
import Control.Effect.Sum
import Control.Effect.Carrier
import Control.Effect.Reader
import Control.Effect.State.Strict
import Control.Effect.Lift
import Data.Text (Text)
import GHC.Generics (Generic, Generic1)
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

{- Example - ghci
:{
  (run . runUserListDb (UserList [User 0 "gus"]) $ renameUser 0 (T.take 2))
  ==
  ( UserList [ User {userId = 0, userName = "gu"} ]
  , Just (User {userId = 0, userName = "gu"})
  )
:}
True
-}

-- handler

newtype UserList = UserList [User]
  deriving (Eq, Show)

newtype UserListDb m a = UserListDb { stateC :: StateC UserList m a }
  deriving newtype (Functor, Applicative, Monad)

runUserListDb :: UserList -> UserListDb m a -> m (UserList, a)
runUserListDb ul uldb = runState ul (stateC uldb)

execUserListDb :: Functor m => UserList -> UserListDb m a -> m UserList
execUserListDb ul uldb = fst <$> runUserListDb ul uldb

evalUserListDb :: Functor m => UserList -> UserListDb m a -> m a
evalUserListDb ul uldb = snd <$> runUserListDb ul uldb

instance (Carrier sig m, Effect sig) => Carrier (UserDb :+: sig) (UserListDb m) where
  eff (L (GetUser uid  k)) =       k =<< (UserListDb $ findUserInList uid <$> get)
  eff (L (PutUser user k)) = const k =<< (UserListDb $ modify (putUserInList user))
  eff (R other)            = send other
  {-# INLINE eff #-}

findUserInList :: Int -> UserList -> Maybe User
findUserInList userId (UserList us) = listToMaybe . filter idMatch $ us
  where idMatch User { userId = userId' } = userId == userId'
putUserInList :: User -> UserList -> UserList
putUserInList user (UserList ulist) = UserList $ go ulist
  where go [] = [user]
        go (u:us) | (userId u == userId user) = user : us
                  | otherwise = u : go us
