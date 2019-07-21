{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , ScopedTypeVariables
  , RankNTypes
  , TypeOperators
  #-}

module ServantTest.HttpApi
  ( api
  , server
  , app
  , ServerConstraints
  ) where

import Data.Proxy
import Servant

import qualified Common.Config.Server as Config
import qualified Common.Version.Server as Version
import qualified ServantTest.HttpApi.User as User

type OpsAPI =
  "ops" :> (
    "version" :> Version.API
    :<|>
    "config" :> Config.API
  )

type OpsServerConstraints m a =
  ( Version.ServerConstraints m a
  , Config.ServerConstraints m
  )

opsServer :: forall m a. OpsServerConstraints m a => ServerT OpsAPI m
opsServer = Version.server
       :<|> Config.server

type ApplicationAPI =
  "api" :> (
    "users" :> User.API
  )

type ApplicationServerConstraints m =
  User.ServerConstraints m

applicationServer :: forall m. ApplicationServerConstraints m => ServerT ApplicationAPI m
applicationServer = User.server

type API = OpsAPI
      :<|> ApplicationAPI

type ServerConstraints m a =
  ( OpsServerConstraints m a
  , ApplicationServerConstraints m
  )

api :: Proxy API
api = Proxy

server :: forall m a. ServerConstraints m a => ServerT API m
server = opsServer
    :<|> applicationServer

app :: forall m a . ServerConstraints m a => (forall x. m x -> Handler x) -> Application
app provideDependencies = serve api $ hoistServer api provideDependencies server
