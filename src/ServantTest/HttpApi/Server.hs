module ServantTest.HttpApi.Server where

import Servant
import ServantTest.HttpApi.ApiType (API)
import qualified ServantTest.HttpApi.User.Server as User

server :: Server API
server = User.server
