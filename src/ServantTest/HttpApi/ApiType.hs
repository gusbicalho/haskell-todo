{-# LANGUAGE
    DataKinds
  , TypeOperators
  #-}

module ServantTest.HttpApi.ApiType where

import Servant.API

import ServantTest.HttpApi.User.ApiType (UserAPI)

type API =
  "api" :> (
    "users" :> UserAPI
  )
