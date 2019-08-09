{-# LANGUAGE TemplateHaskell #-}

{-|
Description : Generic wire types for Auth HttpApi

This module contains types used by the generic Auth HttpApi.
For convenience, you should probably import the "Common.Auth" module, which
exports everything in this module, along with the implementation from
"Common.Auth.HttpApi".

These are type constructors that take a type representing the custom
(domain-dependent) information of a token. The module defines JSON (Aeson) and
JWT (Servant) serialization for these types.

The root type is @'AuthTokenClaims' identityType@, which has a single field
'identity'. This field has the type @'Extensible' identityType@. The
'Extensible' type is serialized in such a way that a token with an identity
field that does not match the @identityType@ will not give an error, but return
an 'Other' value.

In applications, typically code should pattern match on
@('AuthTokenClaims' ('Known' i))@, which ensures the token was parsed correctly
and has a payload with the expected shape.
-}
module Common.Auth.Types where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Servant.Auth.Server
import Data.Text (Text)
import Control.Applicative
import Common.Util.AesonHelpers

{-|
  'Extensible' is a utility wrapper for tolerant token parsing.
  The objective here is that newer versions of a service may generate tokens
  with payloads that are unparseable by older versions. This is specially
  relevant if the service issuing tokens is not the same as the service
  consuming the tokens, and we want to be able to issue new kinds of tokens.

  We want token parsing to degrade gracefully, instead of failing with a parser
  error. With this approach, the application receiving the unknown token may
  even inspect the parsed Aeson 'Value', which will be wrapped in the 'Other'
  constructor.
-}
data Extensible known = Known known
                      | Other Value
  deriving (Eq, Show, Generic)
instance ToJSON known => ToJSON (Extensible known) where
  toJSON (Known known) = toJSON known
  toJSON (Other value) = toJSON value
instance FromJSON known => FromJSON (Extensible  known) where
  parseJSON val = Known <$> parseJSON val
              <|> pure (Other val)
instance ToJSON known => ToJWT (Extensible known)
instance FromJSON known => FromJWT (Extensible known)

{-|
  'AuthTokenClaims' is the top-level payload in the token. Right now, it holds
  only the 'Extensible' identity, but it could evolve to include other fields
  that are useful for tokens of any domain.
-}
newtype AuthTokenClaims identityType = AuthTokenClaims {
  identity :: Extensible identityType
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''AuthTokenClaims)
instance ToJSON identityType => ToJWT (AuthTokenClaims identityType)
instance FromJSON identityType => FromJWT (AuthTokenClaims identityType)

{-|
  'LoginReturn' is the type returned by the login endpoints defined by the Auth HttpApi.
-}
data LoginReturn identityType = LoginReturn { return_identity :: identityType, return_token :: Text } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''LoginReturn)
