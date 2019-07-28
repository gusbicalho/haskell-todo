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
import qualified ServantTest.HttpApi.Auth as Auth
import qualified ServantTest.HttpApi.User as User
import qualified ServantTest.HttpApi.Item as Item

type OpsAPI =
  "ops" :> (
    "config" :> Config.API
  )

type OpsServerConstraints m a =
  ( Config.ServerConstraints m a
  )

opsServer :: forall m a. OpsServerConstraints m a => ServerT OpsAPI m
opsServer = Config.server

type ApplicationAPI =
  "api" :> (
    "version" :> Version.API
    :<|>
    "auth" :> Auth.API
    :<|>
    Auth.JWTAuth :> (
           "users" :> User.API
      :<|> "items" :> Item.API
    )
  )

type ApplicationServerConstraints m a =
  ( Version.ServerConstraints m a
  , User.ServerConstraints m
  , Item.ServerConstraints m
  )

applicationServer :: ApplicationServerConstraints m a => ServerT ApplicationAPI m
applicationServer = Version.server
               :<|> Auth.server
               :<|> \auth -> (
                      User.server auth
                 :<|> Item.server auth
               )

type APIContext = Auth.APIContext

type API = OpsAPI
      :<|> ApplicationAPI

type ServerConstraints m a =
  ( OpsServerConstraints m a
  , ApplicationServerConstraints m a
  )

api :: Proxy API
api = Proxy

apiContext :: Env.Env -> Context APIContext
apiContext = Auth.apiContext

server :: forall m a. ServerConstraints m a => ServerT API m
server = opsServer
    :<|> applicationServer

app :: forall m a . ServerConstraints m a => Env.Env -> (forall x. m x -> Handler x) -> Application
app env provideDependencies =
  serveWithContext api (apiContext env) $ hoistServerWithContext api (Proxy @APIContext) provideDependencies server
