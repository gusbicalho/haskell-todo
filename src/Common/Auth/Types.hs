{-# LANGUAGE TemplateHaskell #-}

module Common.Auth.Types where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Servant.Auth.Server
import Data.Text (Text)
import Control.Applicative
import Common.Util.AesonHelpers

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

newtype AuthTokenClaims identity = AuthTokenClaims {
  identity :: Extensible identity
} deriving (Eq, Show)
$(deriveJSON defaultOptions ''AuthTokenClaims)
instance ToJSON identity => ToJWT (AuthTokenClaims identity)
instance FromJSON identity => FromJWT (AuthTokenClaims identity)

data LoginReturn identity = LoginReturn { return_identity :: identity, return_token :: Text } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''LoginReturn)
