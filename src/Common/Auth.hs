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

I decided to extract all this functionality to @Common@ in order to decouple
Servant logic from the domain of the authentication service as much as
possible. This module stll makes a lot of assumptions - I don't think it's
general enough to actually be a library in the wild. However, I still think it
was worth separating this from the code that actually checks if a
username/password pair (or whatever else your client must provide in order to
get a token) is acceptable.
-}
module Common.Auth (
  -- * Wire types
    AuthTokenClaims (..)
  , Extensible (..)
  , LoginReturn (..)
  -- * Servant HTTP API
  -- ** Authentication API (login)
  , AuthenticationAPI
  , server
  , ServerConstraints
  -- ** Authenticated API
  , AuthenticatedAPI
  -- ** Servant Context
  , APIContextConstraints
  , APIContext
  , apiContext
  ) where

import Common.Auth.Types
import Common.Auth.HttpApi
