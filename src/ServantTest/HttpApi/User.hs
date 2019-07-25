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

type API = QueryParam "sortBy" Wire.User.SortBy :> Get '[JSON] Wire.User.ManyUsers
      :<|> ReqBody '[JSON] Wire.User.NewUserInput :> Post '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> Get '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> Delete '[JSON] Wire.User.SingleUser

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env.Env m
                           )

server :: ServerConstraints m => ServerT API m
server = listUsers
    :<|> createUser
    :<|> getUser
    :<|> deleteUser
  where -- Handlers
    listUsers sortBy = do
        env <- ask
        users <- C.User.listUsers (sorter sortBy) env
        return $ A.User.manyWire users
      where
        sorter Nothing               = id
        sorter (Just Wire.User.Age)  = C.User.sortOnAge
        sorter (Just Wire.User.Name) = C.User.sortOnName

    createUser newUserInput = do
      env <- ask
      user <- C.User.createUser (A.User.inputToNewUser newUserInput) env
      return $ A.User.singleWire user

    getUser idParam = do
        env <- ask
        maybeUser <- C.User.getUser idParam env
        result maybeUser
      where
        result Nothing  = throwError err404
        result (Just x) = return $ A.User.singleWire x

    deleteUser idParam = do
        env <- ask
        maybeUser <- C.User.deleteUser idParam env
        result maybeUser
      where
        result Nothing  = throwError err404
        result (Just x) = return $ A.User.singleWire x
