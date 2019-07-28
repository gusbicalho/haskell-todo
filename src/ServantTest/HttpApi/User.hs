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
import qualified ServantTest.Env as Env
import qualified ServantTest.WireTypes.User as Wire.User
import qualified ServantTest.Controllers.User as C.User
import qualified ServantTest.Adapters.User as A.User

type API = Get '[JSON] Wire.User.ManyUsers
      :<|> ReqBody '[JSON] Wire.User.NewUserInput :> Post '[JSON] Wire.User.SingleUser
      :<|> Capture "userid" Integer :> Get '[JSON] Wire.User.SingleUser

api :: Proxy API
api = Proxy

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env.Env m
                           )

authenticatedAsUser :: Integer -> AuthResult AT.AuthTokenClaims -> Bool
authenticatedAsUser userIdParam (Authenticated AT.AuthTokenClaims { AT.identity = AT.Known AT.User { AT.userId }})
  = userId == userIdParam
authenticatedAsUser _ _ = False

server :: ServerConstraints m => AuthResult AT.AuthTokenClaims -> ServerT API m
server auth = listUsers
         :<|> createUser
         :<|> getUser
  where -- Handlers
    listUsers = do
      env <- ask
      users <- C.User.listUsers env
      return $ A.User.manyWire users

    createUser newUserInput = do
      env <- ask
      user <- C.User.createUser (A.User.inputToNewUser newUserInput) env
      return $ A.User.singleWire user

    getUser idParam
      | not $ authenticatedAsUser idParam auth = throwError err403
      | otherwise = do
          env <- ask
          maybeUser <- C.User.getUser idParam env
          result maybeUser
        where
          result Nothing  = throwError err404
          result (Just x) = return $ A.User.singleWire x
