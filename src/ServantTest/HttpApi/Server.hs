{-# LANGUAGE
    AllowAmbiguousTypes
  , ConstraintKinds
  , DataKinds
  , FlexibleContexts
  , ScopedTypeVariables
  , TypeOperators
  #-}

module ServantTest.HttpApi.Server
  ( api
  , server
  , ServerConstraints
  ) where

import Data.Proxy
import Servant

import qualified Version.Server as Version
import qualified ServantTest.HttpApi.User.Server as User

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
