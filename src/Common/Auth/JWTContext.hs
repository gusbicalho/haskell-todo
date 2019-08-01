{-# LANGUAGE OverloadedLabels #-}

module Common.Auth.JWTContext where

import Servant
import Servant.Auth.Server
import Common.HasVal.Class

type JWTContext = '[ JWTSettings
                   , CookieSettings
                   ]

type JWTContextConstraints env = ( HasVal "jwtSettings" JWTSettings env
                                 , HasVal "cookieSettings" CookieSettings env
                                 )

jwtContext :: JWTContextConstraints env => env -> Context JWTContext
jwtContext env = #jwtSettings env
              :. #cookieSettings env
              :. EmptyContext
