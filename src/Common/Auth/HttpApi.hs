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

type AuthenticatedAPI api = Auth '[JWT] AT.AuthTokenClaims :> api

apiContext :: APIContextConstraints env => env -> Context APIContext
apiContext env = #jwtSettings env
              :. #cookieSettings env
              :. EmptyContext

type AuthenticationAPI = (
         ReqBody '[JSON] AT.LoginInput :> Put '[JSON] AT.LoginReturn
    :<|> ReqBody '[JSON] AT.LoginInput :> Post '[JSON] AT.LoginReturn
  )

type ServerConstraints m env = ( APIContextConstraints env
                               , MonadError ServantErr m
                               , MonadIO m
                               , MonadReader env m
                               )

server :: ServerConstraints m env => (env -> AT.LoginInput -> m (Maybe AT.Identity)) -> ServerT AuthenticationAPI m
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
