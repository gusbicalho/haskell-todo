{-# LANGUAGE OverloadedLabels #-}

module ServantTest.Controllers.User where

import Common.HasVal.Class
import ServantTest.Db.Transactor (Transactor(..))
import ServantTest.Models.User (User(..), NewUser(..))
import ServantTest.Db.User as Db.User

type ControllerConstraints env t m stmt = (HasVal "transactor" env t, Transactor t m stmt, UserDb stmt)

listUsers :: ControllerConstraints env t m stmt => env -> m [User]
listUsers env = do
  let transactor = #transactor env
  transact transactor Db.User.listUsers

getUser :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe User)
getUser idParam env = do
  let transactor = #transactor env
  transact transactor $ Db.User.getUser idParam

createUser :: ControllerConstraints env t m stmt => NewUser -> env -> m User
createUser newUser env = do
  let transactor = #transactor env
  transact transactor $ Db.User.createUser newUser
