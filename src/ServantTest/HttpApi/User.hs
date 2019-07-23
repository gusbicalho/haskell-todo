module ServantTest.HttpApi.User
  ( api
  , server
  , API
  , ServerConstraints
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Servant
import qualified ServantTest.Env as Env
import qualified ServantTest.WireTypes.User as Wire.User
import qualified ServantTest.Controllers.User as C.User
import qualified ServantTest.Adapters.User as A.User

type API = ListUsersAPI
      :<|> GetUserAPI

type ListUsersAPI = QueryParam "sortBy" Wire.User.SortBy :> Get '[JSON] Wire.User.ManyUsers
type GetUserAPI = Capture "userid" Integer :> Get '[JSON] Wire.User.SingleUser

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env.Env m
                           )

server :: ServerConstraints m => ServerT API m
server = listUsers
    :<|> getUser

-- Handlers

listUsers :: ServerConstraints m => ServerT ListUsersAPI m
listUsers Nothing               = A.User.manyWire <$> C.User.listUsers id
listUsers (Just Wire.User.Age)  = A.User.manyWire <$> C.User.listUsers C.User.sortOnAge
listUsers (Just Wire.User.Name) = A.User.manyWire <$> C.User.listUsers C.User.sortOnName

getUser :: ServerConstraints m => ServerT GetUserAPI m
getUser idParam = result =<< C.User.getUser idParam
  where result Nothing  = throwError err404
        result (Just x) = return $ A.User.singleWire x
