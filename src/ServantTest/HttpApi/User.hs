module ServantTest.HttpApi.User
  ( api
  , server
  , API
  , ServerConstraints
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Servant.Auth.Server
import qualified Common.Auth.Types as AT
import qualified Common.Auth.Logic as Auth.Logic
import qualified ServantTest.Env as Env
import qualified ServantTest.WireTypes.User as Wire.User
import qualified ServantTest.Controllers.User as C.User
import qualified ServantTest.Adapters.User as A.User
import qualified ServantTest.WireTypes.Item as Wire.Item
import qualified ServantTest.Controllers.Item as C.Item
import qualified ServantTest.Adapters.Item as A.Item

type API = Get '[JSON] Wire.User.ManyUsers
      :<|> ReqBody '[JSON] Wire.User.NewUserInput :> Post '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> Get '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> "items" :> (
        Get '[JSON] Wire.Item.ManyItems
      )

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env.Env m
                           )

server :: ServerConstraints m => AuthResult AT.AuthTokenClaims -> ServerT API m
server auth = listUsers
         :<|> createUser
         :<|> getUser
         :<|> userItems
  where -- Handlers
    listUsers = do
      env <- ask
      users <- C.User.listUsers env
      return $ A.User.manyWire users

    createUser newUserInput
      | Auth.Logic.authenticated auth = throwError err403
      | otherwise = do
          env <- ask
          user <- C.User.createUser (A.User.inputToNewUser newUserInput) env
          return $ A.User.singleWire user

    getUser idParam
      | not $ Auth.Logic.authenticatedAsUser idParam auth = throwError err403
      | otherwise = do
          env <- ask
          maybeUser <- C.User.getUser idParam env
          result maybeUser
        where
          result Nothing  = throwError err404
          result (Just x) = return $ A.User.singleWire x

    userItems userIdParam
      | not $ Auth.Logic.authenticatedAsUser userIdParam auth = throwError err403
      | otherwise = getItems
        where
          getItems = do
            env <- ask
            items <- C.Item.findItemsByUserId userIdParam env
            return $ A.Item.manyWire items
