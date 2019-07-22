module ServantTest.Controllers.User where

import Prelude hiding (id)
import Data.Maybe (listToMaybe)
import Data.List (sortOn)
import Control.Monad.IO.Class
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

listUsers :: MonadIO m => ([User] -> [User]) -> m [User]
listUsers listTransform = liftIO $ listTransform <$> Db.transact "test.db" Db.listUsers

getUser :: MonadIO m => Integer -> m (Maybe User)
getUser idParam = liftIO . Db.transact "test.db" $ Db.getUser idParam
