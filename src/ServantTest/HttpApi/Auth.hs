module ServantTest.HttpApi.Auth
  ( AuthenticationAPI
  , APIContext
  , server
  , apiContext
  , AuthenticatedAPI
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
import qualified Common.Auth.Logic as Auth.Logic
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
    toToken User { userId } = AT.AuthTokenClaims {
      AT.identity = AT.Known $ AT.User {
        AT.userId = userId
      }
    }

type APIContext = JWTContext
              ++ '[BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)]

type AuthenticatedAPI api = Auth '[JWT] AT.AuthTokenClaims :> api

apiContext :: Env.Env -> Context APIContext
apiContext env = jwtContext env
             .:. basicAuthUser env
              :. EmptyContext

type AuthenticationAPI = "user" :> (
         Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Put '[JSON] AT.Identity
    :<|> Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Post '[JSON] AT.Identity
  )

server :: forall m. MonadError ServantErr m => ServerT AuthenticationAPI m
server = loginUser :<|> loginUser
  where -- Handlers
    loginUser (Authenticated (Auth.Logic.knownIdentity -> Just identity)) = return identity
    loginUser _ = throwError err401
