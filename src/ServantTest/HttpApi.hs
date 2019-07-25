module ServantTest.HttpApi
  ( api
  , server
  , app
  , ServerConstraints
  ) where

import Data.Proxy
import Servant

import qualified ServantTest.Config as Config
import qualified Common.Version.Server as Version
import qualified ServantTest.HttpApi.User as User

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
    "users" :> User.API
  )

type ApplicationServerConstraints m a =
  ( Version.ServerConstraints m a
  , User.ServerConstraints m
  )

applicationServer :: ApplicationServerConstraints m a => ServerT ApplicationAPI m
applicationServer = Version.server :<|> User.server

type API = OpsAPI
      :<|> ApplicationAPI

type ServerConstraints m a =
  ( OpsServerConstraints m a
  , ApplicationServerConstraints m a
  )

api :: Proxy API
api = Proxy

server :: forall m a. ServerConstraints m a => ServerT API m
server = opsServer
    :<|> applicationServer

app :: forall m a . ServerConstraints m a => (forall x. m x -> Handler x) -> Application
app provideDependencies = serve api $ hoistServer api provideDependencies server
