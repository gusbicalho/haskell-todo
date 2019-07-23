module ServantTest.Controllers.User where

import Prelude hiding (id)
import Data.Maybe (listToMaybe)
import Data.List (sortOn)
import Control.Monad.IO.Class
import Control.Monad.Reader
import ServantTest.Models.User (User(..))
import qualified ServantTest.Sqlite as Db

users :: [User]
users = [ User 1 "Isaac Newton" 26 "isaac@newton.com"
        , User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]

sortOnAge :: [User] -> [User]
sortOnAge = sortOn age

sortOnName :: [User] -> [User]
sortOnName = sortOn name

type ControllerConstraints m env t stmt = (MonadReader env m, Db.HasTransactor env t, Db.Transactor t m stmt, Db.UserDb stmt)

listUsers :: ControllerConstraints m env t stmt => ([User] -> [User]) -> m [User]
listUsers listTransform = do
  transactor <- Db.getTransactor <$> ask
  allUsers <- Db.transact transactor Db.listUsers
  return $ listTransform allUsers

getUser :: ControllerConstraints m env t stmt => Integer -> m (Maybe User)
getUser idParam = do
  transactor <- Db.getTransactor <$> ask
  Db.transact transactor $ Db.getUser idParam
