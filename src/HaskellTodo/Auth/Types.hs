{-# LANGUAGE TemplateHaskell #-}

module HaskellTodo.Auth.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Servant.Auth.Server
import qualified Common.Auth as Common
import Common.Util.AesonHelpers

newtype Identity = User  { userId :: Integer }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Identity)
instance ToJWT Identity
instance FromJWT Identity

data LoginInput = LoginInput { input_username :: Text, input_password :: Text } deriving (Eq, Show)
$(deriveJSON defaultOptions { fieldLabelModifier = dropPrefix_ } ''LoginInput)

type IdentityTokenClaims = Common.AuthTokenClaims Identity
