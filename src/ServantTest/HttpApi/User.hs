{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module ServantTest.HttpApi.User
  ( api
  , server
  , API
  , ServerConstraints
  ) where

import Control.Monad.Except
import Servant
import Data.List (sortOn)
import qualified ServantTest.WireTypes.User as U

type API = ListUsersAPI
      :<|> GetUserAPI

type ListUsersAPI = QueryParam "sortBy" U.SortBy :> Get '[JSON] [U.User]
type GetUserAPI = Capture "userid" Integer :> Get '[JSON] U.User

api :: Proxy API
api = Proxy

type ServerConstraints m = MonadError ServantErr m

server :: ServerConstraints m => ServerT API m
server = listUsers
    :<|> getUser

-- Handlers

listUsers :: ServerConstraints m => ServerT ListUsersAPI m
listUsers Nothing       = return users
listUsers (Just U.Age)  = return $ sortOn U.age users
listUsers (Just U.Name) = return $ sortOn U.name users

getUser :: ServerConstraints m => ServerT GetUserAPI m
getUser idParam = result $ filter ((idParam ==) . U.id) $ users
  where result [] = throwError err404
        result (x:_) = return x

users :: [U.User]
users = [ U.User 1 "Isaac Newton" 26 "isaac@newton.com"
        , U.User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]
