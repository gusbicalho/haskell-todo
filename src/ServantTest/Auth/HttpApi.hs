module ServantTest.Auth.HttpApi where

import Servant
import qualified Common.Auth.HttpApi as Auth.HttpApi
import qualified ServantTest.Env as Env
import ServantTest.Auth.WireTypes
import ServantTest.Auth.Adapters
import ServantTest.Controllers.User

type AuthenticationAPI = "user" :> Auth.HttpApi.AuthenticationAPI LoginInput Identity
type AuthenticatedAPI api = Auth.HttpApi.AuthenticatedAPI Identity api
type APIContext = Auth.HttpApi.APIContext

type AuthenticationServerConstraints m = Auth.HttpApi.ServerConstraints m Env.Env

getUserToken :: AuthenticationServerConstraints m => Env.Env -> LoginInput -> m (Maybe Identity)
getUserToken env input = do
    maybeUser <- checkLogin (wireToLoginInput input) env
    return $ toIdentity <$> maybeUser

apiContext :: Env.Env -> Context Auth.HttpApi.APIContext
apiContext = Auth.HttpApi.apiContext

authenticationServer :: AuthenticationServerConstraints m => ServerT AuthenticationAPI m
authenticationServer = Auth.HttpApi.server getUserToken
