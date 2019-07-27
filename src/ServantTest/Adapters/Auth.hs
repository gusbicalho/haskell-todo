module ServantTest.Adapters.Auth where

import Servant
import Data.ByteString.Char8 (unpack)
import Data.String (fromString)
import ServantTest.Models.User as M.User

basicAuthToLoginInput :: BasicAuthData -> M.User.LoginInput
basicAuthToLoginInput (BasicAuthData { basicAuthUsername, basicAuthPassword }) =
  LoginInput ( fromString $ unpack basicAuthUsername )
             ( fromString $ unpack basicAuthPassword )
