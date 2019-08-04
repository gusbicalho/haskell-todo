{-# LANGUAGE OverloadedLabels #-}

module ServantTest.Controllers.User where

import Control.Monad
import Common.HasVal.Class
import Common.Db.Transactor (Transactor(..))
import ServantTest.Models.User (User(..), NewUser(..), LoginInput (..))
import ServantTest.Db.User as Db.User

type ControllerConstraints env t m action = (HasVal "transactor" env t, Transactor t m action, UserDb action)

listUsers :: ControllerConstraints env t m action => env -> m [User]
listUsers env = do
  let transactor = #transactor env
  transact transactor Db.User.listUsers

getUser :: ControllerConstraints env t m action => Integer -> env -> m (Maybe User)
getUser idParam env = do
  let transactor = #transactor env
  transact transactor $ Db.User.getUser idParam

createUser :: ControllerConstraints env t m action => NewUser -> env -> m User
createUser newUser env = do
  let transactor = #transactor env
  transact transactor $ Db.User.createUser newUser

checkLogin :: ControllerConstraints env t m action => LoginInput -> env -> m (Maybe User)
checkLogin (LoginInput login password) env = do
  let transactor = #transactor env
  maybeUser <- transact transactor $ Db.User.findUserByLogin login
  return $ do user <- maybeUser
              guard (password == userPassword user)
              return user
