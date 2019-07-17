{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module ServantTest.HttpApi.User.Server where

import Servant
import Data.List (sortOn)
import qualified ServantTest.HttpApi.User.ApiType as AT

server :: Server AT.UserAPI
server = listUsers
    :<|> getUser

listUsers :: Server AT.ListUsersAPI
listUsers Nothing       = return users
listUsers (Just AT.Age)  = return $ sortOn AT.age users
listUsers (Just AT.Name) = return $ sortOn AT.name users

getUser :: Server AT.GetUserAPI
getUser idParam = result $ filter ((idParam ==) . AT.id) $ users
  where result [] = throwError err404
        result (x:_) = return x

users :: [AT.User]
users = [ AT.User 1 "Isaac Newton" 26 "isaac@newton.com"
        , AT.User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]
