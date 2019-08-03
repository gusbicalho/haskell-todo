module ServantTest.Auth.HttpApi where

import Servant
import qualified Common.Auth as Common
import qualified ServantTest.Env as Env
import ServantTest.Auth.Types
import ServantTest.Auth.Adapters
import ServantTest.Controllers.User

type AuthenticationAPI = "user" :> Common.AuthenticationAPI LoginInput Identity
type AuthenticatedAPI api = Common.AuthenticatedAPI Identity api
type APIContext = Common.APIContext

type AuthenticationServerConstraints m = Common.ServerConstraints m Env.Env

getUserToken :: AuthenticationServerConstraints m => Env.Env -> LoginInput -> m (Maybe Identity)
getUserToken env input = do
    maybeUser <- checkLogin (wireToLoginInput input) env
    return $ toIdentity <$> maybeUser

apiContext :: Env.Env -> Context Common.APIContext
apiContext = Common.apiContext

authenticationServer :: AuthenticationServerConstraints m => ServerT AuthenticationAPI m
authenticationServer = Common.server getUserToken
