module ServantTest.Controllers.User where

import Prelude hiding (id)
import Data.List (sortOn)
import Common.HasVal.Class
import ServantTest.Models.User (User(..), NewUser(..))
import ServantTest.Db.Transactor (Transactor(..))
import ServantTest.Db.User as Db.User

sortOnAge :: [User] -> [User]
sortOnAge = sortOn userAge

sortOnName :: [User] -> [User]
sortOnName = sortOn userName

type ControllerConstraints env t m stmt = (HasVal "transactor" t env, Transactor t m stmt, UserDb stmt)

listUsers :: forall env t m stmt. ControllerConstraints env t m stmt => ([User] -> [User]) -> env -> m [User]
listUsers listTransform env = do
  let transactor = getVal @"transactor" env
  allUsers <- transact transactor Db.User.listUsers
  return $ listTransform allUsers

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
