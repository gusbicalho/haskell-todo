{-# LANGUAGE OverloadedLabels #-}

module ServantTest.HttpApi.Auth
  ( apiContext
  , Auth.HttpApi.APIContext
  , AuthenticationAPI
  , AuthenticationServerConstraints
  , authenticationServer
  , Auth.HttpApi.AuthenticatedAPI
  ) where

import Data.String (fromString)
import qualified Data.Text as T
import Servant
import qualified Common.Auth.Types as AT
import qualified Common.Auth.HttpApi as Auth.HttpApi
import qualified ServantTest.Env as Env
import ServantTest.Models.User
import ServantTest.Controllers.User

apiContext :: Env.Env -> Context Auth.HttpApi.APIContext
apiContext = Auth.HttpApi.apiContext

type AuthenticationAPI = "user" :> Auth.HttpApi.AuthenticationAPI

type AuthenticationServerConstraints m = Auth.HttpApi.ServerConstraints m Env.Env

authenticationServer :: AuthenticationServerConstraints m => ServerT AuthenticationAPI m
authenticationServer = Auth.HttpApi.server authFn
  where
    authFn env input = do
        maybeUser <- checkLogin (wireToUserLoginInput input) env
        return $ toToken <$> maybeUser
    wireToUserLoginInput (AT.LoginInput username password) =
      LoginInput ( fromString $ T.unpack username )
                        ( fromString $ T.unpack password )
    toToken User { userId } = AT.User { AT.userId = userId }
