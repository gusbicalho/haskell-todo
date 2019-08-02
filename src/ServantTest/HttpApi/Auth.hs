{-# LANGUAGE OverloadedLabels #-}

module ServantTest.HttpApi.Auth
  ( apiContext
  , Auth.HttpApi.APIContext
  , AuthenticationAPI
  , AuthenticationServerConstraints
  , authenticationServer
  , Auth.HttpApi.AuthenticatedAPI
  ) where

import Servant
import qualified Common.Auth.Types as AT
import qualified Common.Auth.HttpApi as Auth.HttpApi
import qualified ServantTest.Env as Env
import ServantTest.Models.User
import ServantTest.Adapters.Auth
import ServantTest.Controllers.User

getUserToken :: Env.Env -> BasicAuthData -> IO (Maybe AT.AuthTokenClaims)
getUserToken env basicAuthData = do
    maybeUser <- checkLogin (basicAuthToLoginInput basicAuthData) env
    return $ toToken <$> maybeUser
  where
    toToken User { userId } = AT.AuthTokenClaims {
      AT.identity = AT.Known $ AT.User {
        AT.userId = userId
      }
    }

apiContext :: Env.Env -> Context Auth.HttpApi.APIContext
apiContext = Auth.HttpApi.apiContext getUserToken

type AuthenticationAPI = "user" :> Auth.HttpApi.AuthenticationAPI

type AuthenticationServerConstraints m = Auth.HttpApi.ServerConstraints m Env.Env

authenticationServer :: AuthenticationServerConstraints m => ServerT AuthenticationAPI m
authenticationServer = Auth.HttpApi.server
