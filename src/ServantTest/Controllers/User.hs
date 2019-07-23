module ServantTest.Controllers.User where

import Prelude hiding (id)
import Data.List (sortOn)
import ServantTest.Models.User (User(..), NewUser(..))
import ServantTest.Db.Transactor (Transactor(..), HasTransactor(..))
import ServantTest.Db.User as Db.User

users :: [User]
users = [ User 1 "Isaac Newton" 26 "isaac@newton.com"
        , User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]

sortOnAge :: [User] -> [User]
sortOnAge = sortOn userAge

sortOnName :: [User] -> [User]
sortOnName = sortOn userName

type ControllerConstraints env t m stmt = (HasTransactor env t, Transactor t m stmt, UserDb stmt)

listUsers :: ControllerConstraints env t m stmt => ([User] -> [User]) -> env -> m [User]
listUsers listTransform env = do
  let transactor = getTransactor env
  allUsers <- transact transactor Db.User.listUsers
  return $ listTransform allUsers

getUser :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe User)
getUser idParam env = do
  let transactor = getTransactor env
  transact transactor $ Db.User.getUser idParam

createUser :: ControllerConstraints env t m stmt => NewUser -> env -> m User
createUser newUser env = do
  let transactor = getTransactor env
  transact transactor $ Db.User.createUser newUser

deleteUser :: ControllerConstraints env t m stmt => Integer -> env -> m (Maybe User)
deleteUser idParam env = do
  let transactor = getTransactor env
  transact transactor $ Db.User.deleteUser idParam
