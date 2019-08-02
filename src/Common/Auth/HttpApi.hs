{-# LANGUAGE OverloadedLabels #-}

module Common.Auth.HttpApi
  ( AuthenticationAPI
  , server
  , APIContext
  , APIContextConstraints
  , apiContext
  , AuthenticatedAPI
  , ServerConstraints
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
import Common.HasVal.Class
import qualified Common.Auth.Types as AT
import qualified Common.Auth.Logic as Auth.Logic

basicAuth :: (BasicAuthData -> IO (Maybe AT.AuthTokenClaims)) -> BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)
basicAuth authFn basicAuthData = do
    maybeToken <- authFn basicAuthData
    maybe (return SAS.Indefinite) (return . SAS.Authenticated) maybeToken

type APIContextConstraints env = ( HasVal "jwtSettings" env JWTSettings
                                 , HasVal "cookieSettings" env CookieSettings
                                 )

type APIContext = '[ JWTSettings
                   , CookieSettings
                   , BasicAuthData -> IO (AuthResult AT.AuthTokenClaims)
                   ]

type AuthenticatedAPI api = Auth '[JWT] AT.AuthTokenClaims :> api

apiContext :: APIContextConstraints env => (env -> BasicAuthData -> IO (Maybe AT.AuthTokenClaims)) -> env -> Context APIContext
apiContext authFn env = #jwtSettings env
                     :. #cookieSettings env
                     :. basicAuth (authFn env)
                     :. EmptyContext

data LoginReturn = LoginReturn { identity :: AT.Identity, token :: T.Text } deriving (Eq, Show, Generic)
instance FromJSON LoginReturn where
instance ToJSON LoginReturn where

type AuthenticationAPI = (
         Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Put '[JSON] LoginReturn
    :<|> Auth '[JWT, SA.BasicAuth] AT.AuthTokenClaims :> Post '[JSON] LoginReturn
  )

type ServerConstraints m env = ( APIContextConstraints env
                               , MonadError ServantErr m
                               , MonadIO m
                               , MonadReader env m
                               )

server :: ServerConstraints m env => ServerT AuthenticationAPI m
server = login :<|> login
  where -- Handlers
    login (Authenticated claims@(Auth.Logic.knownIdentity -> Just identity)) = do
      jwtSettings <- asks #jwtSettings
      jwt <- liftIO $ makeJWT claims jwtSettings Nothing
      case jwt of
        Left _ -> throwError err500
        Right tok -> return $ LoginReturn identity (T.pack $ BS.unpack tok)
    login _ = throwError err401
