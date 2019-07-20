{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings
  #-}
module ServantTest.HttpApi.User
  ( api
  , server
  , AT.API
  , ServerConstraints
  ) where

import Control.Monad.Except
import Servant
import Data.List (sortOn)
import qualified ServantTest.HttpApi.User.ApiType as AT

api :: Proxy AT.API
api = Proxy

type ServerConstraints m = MonadError ServantErr m

server :: ServerConstraints m => ServerT AT.API m
server = listUsers
    :<|> getUser

listUsers :: ServerConstraints m => ServerT AT.ListUsersAPI m
listUsers Nothing       = return users
listUsers (Just AT.Age)  = return $ sortOn AT.age users
listUsers (Just AT.Name) = return $ sortOn AT.name users

getUser :: ServerConstraints m => ServerT AT.GetUserAPI m
getUser idParam = result $ filter ((idParam ==) . AT.id) $ users
  where result [] = throwError err404
        result (x:_) = return x

users :: [AT.User]
users = [ AT.User 1 "Isaac Newton" 26 "isaac@newton.com"
        , AT.User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]
