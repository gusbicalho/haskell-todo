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
    listUsers sortBy = ask >>= \env ->
        A.User.manyWire <$> C.User.listUsers (sorter sortBy) env
      where sorter Nothing               = id
            sorter (Just Wire.User.Age)  = C.User.sortOnAge
            sorter (Just Wire.User.Name) = C.User.sortOnName

    createUser newUserInput = ask >>= \env ->
      A.User.singleWire <$> C.User.createUser (A.User.inputToNewUser newUserInput) env

    getUser idParam = ask >>= \env ->
      C.User.getUser idParam env >>= result
      where result Nothing  = throwError err404
            result (Just x) = return $ A.User.singleWire x

    deleteUser idParam = ask >>= \env ->
      C.User.deleteUser idParam env >>= result
      where result Nothing  = throwError err404
            result (Just x) = return $ A.User.singleWire x
