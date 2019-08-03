module ServantTest.Auth.Logic where

import Servant.Auth.Server
import ServantTest.Auth.WireTypes

knownIdentity :: IdentityTokenClaims -> Maybe Identity
knownIdentity AuthTokenClaims { identity = Known identity } = Just identity
knownIdentity _ = Nothing

resultToIdentity :: AuthResult IdentityTokenClaims -> Maybe Identity
resultToIdentity (Authenticated claims) = knownIdentity claims
resultToIdentity _                      = Nothing

authenticatedAs :: (Identity -> Bool) -> AuthResult IdentityTokenClaims -> Bool
authenticatedAs predicate (resultToIdentity -> Just identity) = predicate identity
authenticatedAs _         _                                   = False

isUser :: Integer -> Identity -> Bool
isUser userIdParam User { userId } = userIdParam == userId

authenticatedAsUser :: Integer -> AuthResult IdentityTokenClaims -> Bool
authenticatedAsUser userIdParam = authenticatedAs (isUser userIdParam)

authenticated :: AuthResult IdentityTokenClaims -> Bool
authenticated (Authenticated _) = True
authenticated _                 = False

authenticatedUserId :: AuthResult IdentityTokenClaims -> Maybe Integer
authenticatedUserId = fmap userId . resultToIdentity
