{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module Server.User where

import Servant
import Data.List (sortOn)
import qualified ApiType.User as U

server :: Server U.UserAPI
server = listUsers
    :<|> getUser

listUsers :: Server U.ListUsersAPI
listUsers Nothing       = return users
listUsers (Just U.Age)  = return $ sortOn U.age users
listUsers (Just U.Name) = return $ sortOn U.name users

getUser :: Server U.GetUserAPI
getUser idParam = result $ filter ((idParam ==) . U.id) $ users
  where result [] = throwError err404
        result (x:_) = return x

users :: [U.User]
users = [ U.User 1 "Isaac Newton" 26 "isaac@newton.com"
        , U.User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]
