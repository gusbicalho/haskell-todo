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
import qualified HaskellTodo.Env as Env
import qualified Common.Version.Server as Version
import qualified HaskellTodo.HttpApi.Static as Static
import qualified HaskellTodo.Auth.HttpApi as Auth
import qualified HaskellTodo.HttpApi.User as User
import qualified HaskellTodo.HttpApi.Item as Item

type ServerConstraints m a =
  ( Config.ServerConstraints m a
  , Version.ServerConstraints m a
  , User.ServerConstraints m
  , Item.ServerConstraints m
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

server :: ServerConstraints m a => ServerT API m
server =
  -- /ops
  Config.server
  :<|> ( -- /api
         Version.server
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

apiContext :: Env.Env -> Context APIContext
apiContext = Auth.apiContext

app :: forall m a . ServerConstraints m a => Env.Env -> (forall x. m x -> Handler x) -> Application
app env provideDependencies =
  serveWithContext api (apiContext env) $ hoistServerWithContext api (Proxy @APIContext) provideDependencies server
