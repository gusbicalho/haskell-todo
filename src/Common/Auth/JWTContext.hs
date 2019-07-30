module Common.Auth.JWTContext where

import Servant
import Servant.Auth.Server
import Crypto.JOSE (JWK)
import Common.HasVal.Class

type JWTContext = '[ JWTSettings
                   , CookieSettings
                   ]

type JWTContextConstraints env = HasVal "jwtSettings" JWTSettings env

jwtContext :: JWTContextConstraints env => env -> Context JWTContext
jwtContext env = getVal @"jwtSettings" env
              :. defaultCookieSettings
              :. EmptyContext
