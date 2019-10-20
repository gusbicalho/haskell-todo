module HaskellTodo.Auth.HttpApi where

import Servant
import qualified Common.Auth as Common
import qualified HaskellTodo.Env as Env
import HaskellTodo.Auth.Types
import HaskellTodo.Auth.Adapters
import HaskellTodo.Controllers.User

type AuthenticationAPI = "user" :> Common.AuthenticationAPI LoginInput Identity
type AuthenticatedAPI api = Common.AuthenticatedAPI Identity api
type APIContext = Common.APIContext

type AuthenticationServerConstraints sig m = Common.ServerConstraints Env.Env sig m

getUserToken :: AuthenticationServerConstraints sig m => Env.Env -> LoginInput -> m (Maybe Identity)
getUserToken env input = do
    maybeUser <- checkLogin (wireToLoginInput input) env
    return $ toIdentity <$> maybeUser

apiContext :: Env.Env -> Context Common.APIContext
apiContext = Common.apiContext

authenticationServer :: AuthenticationServerConstraints sig m => ServerT AuthenticationAPI m
authenticationServer = Common.server getUserToken
