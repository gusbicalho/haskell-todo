{-|
Description : Generic authentication HTTP API in Servant

This module implements a generic Http API for authentication, using Servant.
Types for token and request bodies are defined in @Common.Auth.Types@ and
re-exported here. Actual implementation of the Servant API is in
@Common.Auth.HttpApi@, also re-exported by this module.

It exports the @AuthenticationAPI@ type constructor, that returns a Servant API
type with endpoints for user login. Correspondingly, the @server@ exported
function takes an authentication function and builds a Servant server matching
the @AuthenticationAPI@.

The AuthenticationAPI requires an environment type that provides Servant
JWTSettings and CookieSettings, by implementing @Common.HasVal.Class@.

The @AuthenticatedAPI@ type constructor wraps a Servant API in a JWT
authentication combinator.

Both @AuthenticationAPI@ and @AuthenticatedAPI@ require a Servant context
that provide JWTSettings and CookieSettings. The exported @apiContext@ function
can build such a context, given an environment that provides these settings
via @Common.HasVal.Class@.
-}
module Common.Auth
  ( module Common.Auth.Types
  , module Common.Auth.HttpApi
  ) where

import Common.Auth.Types ( AuthTokenClaims (..)
                         , Extensible (..)
                         , LoginReturn (..)
                         )
import Common.Auth.HttpApi ( AuthenticationAPI
                           , server
                           , ServerConstraints
                           , AuthenticatedAPI
                           , APIContextConstraints
                           , APIContext
                           , apiContext
                           )
