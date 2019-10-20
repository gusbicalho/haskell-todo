{-|
Description: Top-level Servant HTTP API

This file just composes several the Servant APIs from @HaskellTodo.HttpApi.*@
modules into a single API.

One interesting limitation of Servant.Auth shows up here in the way we have to
explicitly pass the @auth@ argument to 'User.server' and 'Item.server'. This
happens because the 'Auth.AuthenticatedAPI' type (which uses "Servant.Auth")
must be matched by a function that takes an argument of type
'Common.Auth.AuthTokenClaims' and returns a server. Both 'User.server' and
'Item.server', individually, have the required type, but their combination
using ':<|>' does not.
-}
module HaskellTodo.HttpApi
  ( api
  , server
  , app
  , ServerConstraints
  ) where

import Data.Proxy
import Servant

import qualified HaskellTodo.Config as Config
import           HaskellTodo.Env (Env)
import qualified Common.Version.Server as Version
import qualified HaskellTodo.HttpApi.Static as Static
import qualified HaskellTodo.Auth.HttpApi as Auth
import qualified HaskellTodo.HttpApi.User as User
import qualified HaskellTodo.HttpApi.Item as Item

type ServerConstraints sig m =
  ( Config.ServerConstraints Env sig m
  , Version.ServerConstraints Env sig m
  , Auth.AuthenticationServerConstraints sig m
  , User.ServerConstraints sig m
  , Item.ServerConstraints sig m
  )

type API =
  "ops" :> (
    "config" :> Config.API
  )
  :<|>
  "api" :> (
    "version" :> Version.API
    :<|>
    "auth" :> Auth.AuthenticationAPI
    :<|>
    Auth.AuthenticatedAPI (
           "users" :> User.API
      :<|> "items" :> Item.API
    )
  )
  :<|>
  Static.API

server :: ServerConstraints sig m => ServerT API m
server =
  -- /ops
  Config.server @Env
  :<|> ( -- /api
         Version.server @Env
    :<|> Auth.authenticationServer
    :<|> \auth ->
           User.server auth
      :<|> Item.server auth
  )
  :<|> -- static files
  Static.server


type APIContext = Auth.APIContext

api :: Proxy API
api = Proxy

apiContext :: Env -> Context APIContext
apiContext = Auth.apiContext

app :: forall sig m . ServerConstraints sig m => Env -> (forall x. m x -> Handler x) -> Application
app env provideDependencies =
  serveWithContext api (apiContext env) $ hoistServerWithContext api (Proxy @APIContext) provideDependencies server
