module Common.Auth.JWTContext where

import Servant
import Servant.Auth.Server
import Crypto.JOSE (JWK)
import Common.HasVal.Class

type JWTContext = '[ JWTSettings
                   , CookieSettings
                   ]

type JWTContextConstraints env = HasVal "jwtKey" JWK env

jwtContext :: JWTContextConstraints env => env -> Context JWTContext
jwtContext env = defaultJWTSettings (getVal @"jwtKey" env)
              :. defaultCookieSettings
              :. EmptyContext
