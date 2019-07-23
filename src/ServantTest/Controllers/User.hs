module ServantTest.Controllers.User where

import Prelude hiding (id)
import Data.List (sortOn)
import Control.Monad.Reader
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

type ControllerConstraints m env t stmt = (MonadReader env m, HasTransactor env t, Transactor t m stmt, UserDb stmt)

listUsers :: ControllerConstraints m env t stmt => ([User] -> [User]) -> m [User]
listUsers listTransform = do
  transactor <- getTransactor <$> ask
  allUsers <- transact transactor Db.User.listUsers
  return $ listTransform allUsers

getUser :: ControllerConstraints m env t stmt => Integer -> m (Maybe User)
getUser idParam = do
  transactor <- getTransactor <$> ask
  transact transactor $ Db.User.getUser idParam

createUser :: ControllerConstraints m env t stmt => NewUser -> m User
createUser newUser = do
  transactor <- getTransactor <$> ask
  transact transactor $ Db.User.createUser newUser

deleteUser :: ControllerConstraints m env t stmt => Integer -> m (Maybe User)
deleteUser idParam = do
  transactor <- getTransactor <$> ask
  transact transactor $ Db.User.deleteUser idParam
