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
    listUsers Nothing               = A.User.manyWire <$> C.User.listUsers id
    listUsers (Just Wire.User.Age)  = A.User.manyWire <$> C.User.listUsers C.User.sortOnAge
    listUsers (Just Wire.User.Name) = A.User.manyWire <$> C.User.listUsers C.User.sortOnName

    createUser newUserInput = A.User.singleWire <$> C.User.createUser (A.User.inputToNewUser newUserInput)

    getUser idParam = result =<< C.User.getUser idParam
      where result Nothing  = throwError err404
            result (Just x) = return $ A.User.singleWire x

    deleteUser idParam = result =<< C.User.deleteUser idParam
      where result Nothing  = throwError err404
            result (Just x) = return $ A.User.singleWire x
