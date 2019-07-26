module ServantTest.Controllers.User where

import Common.HasVal.Class
import ServantTest.Models.User (User(..), NewUser(..))
import ServantTest.Db.Transactor (Transactor(..))
import ServantTest.Db.User as Db.User

type ControllerConstraints env t m stmt = (HasVal "transactor" t env, Transactor t m stmt, UserDb stmt)

listUsers :: forall env t m stmt. ControllerConstraints env t m stmt => env -> m [User]
listUsers env = do
  let transactor = getVal @"transactor" env
  transact transactor Db.User.listUsers

getUser :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe User)
getUser idParam env = do
  let transactor = getVal @"transactor" env
  transact transactor $ Db.User.getUser idParam

createUser :: ControllerConstraints env t m stmt => NewUser -> env -> m User
createUser newUser env = do
  let transactor = getVal @"transactor" env
  transact transactor $ Db.User.createUser newUser

deleteUser :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe User)
deleteUser idParam env = do
  let transactor = getVal @"transactor" env
  transact transactor $ Db.User.deleteUser idParam
