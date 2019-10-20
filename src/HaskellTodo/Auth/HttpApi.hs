module HaskellTodo.Auth.HttpApi where

import Servant
import qualified Common.Auth as Common
import HaskellTodo.Env
import HaskellTodo.Auth.Types
import HaskellTodo.Auth.Adapters
import HaskellTodo.Controllers.User

type AuthenticationAPI = "user" :> Common.AuthenticationAPI LoginInput Identity
type AuthenticatedAPI api = Common.AuthenticatedAPI Identity api
type APIContext = Common.APIContext

type AuthenticationServerConstraints sig m = Common.ServerConstraints Env sig m

getUserToken :: ( AuthenticationServerConstraints sig m
                , EnvRuntimeConstraints m
                ) => Env -> LoginInput -> m (Maybe Identity)
getUserToken env input = do
    maybeUser <- checkLogin (wireToLoginInput input) env
    return $ toIdentity <$> maybeUser

apiContext :: Env -> Context Common.APIContext
apiContext = Common.apiContext

authenticationServer :: ( AuthenticationServerConstraints sig m
                        , EnvRuntimeConstraints m
                        ) => ServerT AuthenticationAPI m
authenticationServer = Common.server getUserToken
