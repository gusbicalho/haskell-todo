module Common.Auth.Logic where

import Servant.Auth.Server
import qualified Common.Auth.Types as AT

authenticatedAsUser :: Integer -> AuthResult AT.AuthTokenClaims -> Bool
authenticatedAsUser userIdParam (Authenticated AT.AuthTokenClaims { AT.identity = AT.Known AT.User { AT.userId }})
  = userId == userIdParam
authenticatedAsUser _ _ = False

authenticated :: AuthResult AT.AuthTokenClaims -> Bool
authenticated (Authenticated _) = True
authenticated _                 = False

authenticatedUserId :: AuthResult AT.AuthTokenClaims -> Maybe Integer
authenticatedUserId (Authenticated AT.AuthTokenClaims { AT.identity = AT.Known AT.User { AT.userId }})
  = Just userId
authenticatedUserId _ = Nothing

knownIdentity :: AT.AuthTokenClaims -> Maybe AT.Identity
knownIdentity AT.AuthTokenClaims { AT.identity = AT.Known identity } = Just identity
knownIdentity _ = Nothing
