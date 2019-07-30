module ServantTest.HttpApi
  ( api
  , server
  , app
  , ServerConstraints
  ) where

import Data.Proxy
import Servant

import qualified ServantTest.Config as Config
import qualified ServantTest.Env as Env
import qualified Common.Version.Server as Version
import qualified ServantTest.HttpApi.Static as Static
import qualified ServantTest.HttpApi.Auth as Auth
import qualified ServantTest.HttpApi.User as User
import qualified ServantTest.HttpApi.Item as Item

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
  ( -- /ops
    Config.server
  )
  :<|> ( -- /api
         Version.server
    :<|> Auth.server
    :<|> \auth -> (
           User.server auth
      :<|> Item.server auth
    )
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
