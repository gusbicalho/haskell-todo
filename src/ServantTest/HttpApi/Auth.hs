module ServantTest.HttpApi.Auth where
  -- ( api
  -- , server
  -- , API
  -- , ServerConstraints
  -- )
  -- where

import Control.Monad.Except
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import qualified Common.Auth.Types as AT
import Common.HasVal.Class
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

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)

instance FromBasicAuthData AT.AuthTokenClaims where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type APIContext = '[ JWTSettings
                   , CookieSettings
                   , BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)
                   ]

apiContext :: Env.Env -> Context APIContext
apiContext env = defaultJWTSettings (getVal @"jwtKey" env)
              :. defaultCookieSettings
              :. basicAuthUser env
              :. EmptyContext

type API = "user" :> Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Put '[JSON] ()

api :: Proxy API
api = Proxy

server :: forall m. MonadError ServantErr m => ServerT API m
server = loginUser
  where -- Handlers
    loginUser (Authenticated _) = return ()
    loginUser _                 = throwError err401
