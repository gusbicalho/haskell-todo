{-# LANGUAGE TemplateHaskell #-}

module Common.Auth.Types where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Servant.Auth.Server
import Data.Text (Text)
import Control.Applicative
import Common.Util.AesonHelpers

newtype Scope = Scope Text deriving (Eq, Show)
$(deriveJSON defaultOptions ''Scope)
instance ToJWT Scope
instance FromJWT Scope

data Identity = User  { userId :: Integer
                      }
              | Admin { adminId :: Text
                      , scopes :: [Scope]
                      }
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''Identity)
instance ToJWT Identity
instance FromJWT Identity

data ExtensibleIdentity = Known Identity
                        | Other Value
  deriving (Eq, Show, Generic)
instance ToJSON ExtensibleIdentity where
  toJSON (Known identity) = toJSON identity
  toJSON (Other value)    = toJSON value
instance FromJSON ExtensibleIdentity where
  parseJSON val = Known <$> parseJSON val
              <|> (pure $ Other val)
instance ToJWT ExtensibleIdentity
instance FromJWT ExtensibleIdentity

data AuthTokenClaims = AuthTokenClaims {
  identity :: ExtensibleIdentity
} deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''AuthTokenClaims)
instance ToJWT AuthTokenClaims
instance FromJWT AuthTokenClaims

data LoginReturn = LoginReturn { return_identity :: Identity, return_token :: Text } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''LoginReturn)

data LoginInput = LoginInput { input_username :: Text, input_password :: Text } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''LoginInput)
