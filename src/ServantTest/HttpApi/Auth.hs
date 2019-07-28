module ServantTest.HttpApi.Auth
  ( API
  , APIContext
  , server
  , apiContext
  , JWTAuth
  , JWTContext
  , jwtContext
  , JWTContextConstraints
  , basicAuthUser
  ) where

import Control.Monad.Except
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import qualified Common.Auth.Types as AT
import Common.Auth.JWTContext (JWTContext, JWTContextConstraints, jwtContext)
import Common.Util.ServantHelpers ((.:.), type (++))
import qualified ServantTest.Env as Env
import ServantTest.Models.User
import ServantTest.Adapters.Auth
import ServantTest.Controllers.Auth

basicAuthUser :: Env.Env -> BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)
basicAuthUser env basicAuthData = do
    maybeUser <- checkUserLogin (basicAuthToLoginInput basicAuthData) env
    maybe (return SAS.Indefinite) (return . SAS.Authenticated . toToken) maybeUser
  where
    toToken (User { userId }) = AT.AuthTokenClaims {
      AT.identity = AT.Known $ AT.User {
        AT.userId = userId
      }
    }

type APIContext = JWTContext
              ++ '[BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)]

type JWTAuth = Auth '[JWT] AT.AuthTokenClaims

apiContext :: Env.Env -> Context APIContext
apiContext env = jwtContext env
             .:. basicAuthUser env
              :. EmptyContext

type API = "user" :> Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Put '[JSON] ()

server :: forall m. MonadError ServantErr m => ServerT API m
server = loginUser
  where -- Handlers
    loginUser (Authenticated auth) = return ()
    loginUser _                 = throwError err401
