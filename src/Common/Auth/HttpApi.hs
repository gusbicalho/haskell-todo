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
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Common.HasVal.Class
import qualified Common.Auth.Types as AT

type APIContextConstraints env = ( HasVal "jwtSettings" env JWTSettings
                                 , HasVal "cookieSettings" env CookieSettings
                                 )

type APIContext = '[ JWTSettings
                   , CookieSettings
                   ]

type AuthenticatedAPI identity api = Auth '[JWT] (AT.AuthTokenClaims identity) :> api

apiContext :: APIContextConstraints env => env -> Context APIContext
apiContext env = #jwtSettings env
              :. #cookieSettings env
              :. EmptyContext

type AuthenticationAPI input identity = (
         ReqBody '[JSON] input :> Put '[JSON] (AT.LoginReturn identity)
    :<|> ReqBody '[JSON] input :> Post '[JSON] (AT.LoginReturn identity)
  )

type ServerConstraints m env = ( APIContextConstraints env
                               , MonadError ServantErr m
                               , MonadIO m
                               , MonadReader env m
                               )

server :: (ServerConstraints m env, ToJSON identity)
          => (env -> input -> m (Maybe identity))
          -> ServerT (AuthenticationAPI input identity) m
server authFn = login :<|> login
  where -- Handlers
    login input = do
      env <- ask
      maybeIdentity <- authFn env input
      identity <- case maybeIdentity of
                    Nothing -> throwError err401
                    Just identity -> return identity
      let claims = AT.AuthTokenClaims $ AT.Known identity
      jwtSettings <- asks #jwtSettings
      jwt <- liftIO $ makeJWT claims jwtSettings Nothing
      case jwt of
        Left _ -> throwError err500
        Right tok -> return $ AT.LoginReturn identity (T.pack $ BS.unpack tok)
