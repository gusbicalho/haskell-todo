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

import qualified Common.Version.Server as Version
import qualified ServantTest.HttpApi.User as User

type API =
  "api" :> (
       "version" :> Version.API
  :<|> "users" :> User.API
  )

api :: Proxy API
api = Proxy

type ServerConstraints m a = (Version.ServerConstraints m a, User.ServerConstraints m)

server :: forall m a. ServerConstraints m a => ServerT API m
server = Version.server
    :<|> User.server

app :: forall m a . ServerConstraints m a => (forall x. m x -> Handler x) -> Application
app provideDependencies = serve api $ hoistServer api provideDependencies server
