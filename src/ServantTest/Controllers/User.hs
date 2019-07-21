module ServantTest.Controllers.User where

import Prelude hiding (id)
import Data.Maybe (listToMaybe)
import Data.List (sortOn)
import Control.Monad.IO.Class
import ServantTest.Models.User (User(..))

users :: [User]
users = [ User 1 "Isaac Newton" 26 "isaac@newton.com"
        , User 2 "Albert Einstein" 42 "albert@einstein.com"
        ]

sortOnAge :: [User] -> [User]
sortOnAge = sortOn age

sortOnName :: [User] -> [User]
sortOnName = sortOn name

listUsers :: MonadIO m => ([User] -> [User]) -> m [User]
listUsers listTransform = return $ listTransform users

getUser :: MonadIO m => Integer -> m (Maybe User)
getUser idParam = return . listToMaybe . filter ((idParam ==) . id) $ users
