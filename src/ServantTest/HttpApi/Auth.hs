{-# LANGUAGE OverloadedLabels #-}

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
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import GHC.Generics
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

data LoginReturn = LoginReturn { identity :: AT.Identity, token :: T.Text } deriving (Eq, Show, Generic)
instance FromJSON LoginReturn where
instance ToJSON LoginReturn where

type AuthenticationAPI = "user" :> (
         Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Put '[JSON] LoginReturn
    :<|> Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Post '[JSON] LoginReturn
  )

type ServerConstraints m = ( MonadError ServantErr m
                           , MonadIO m
                           , MonadReader Env.Env m
                           )

server :: ServerConstraints m => ServerT AuthenticationAPI m
server = loginUser :<|> loginUser
  where -- Handlers
    loginUser (Authenticated claims@(Auth.Logic.knownIdentity -> Just identity)) = do
      jwtSettings <- asks #jwtSettings
      jwt <- liftIO $ makeJWT claims jwtSettings Nothing
      case jwt of
        Left _ -> throwError err500
        Right tok -> return $ LoginReturn identity (T.pack $ BS.unpack tok)
    loginUser _ = throwError err401
