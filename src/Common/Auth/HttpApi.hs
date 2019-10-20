{-|
Description : Generic authentication HTTP API in Servant

This module implements a generic Http API for authentication, using Servant.
For convenience, you should probably import the "Common.Auth" module, which
exports everything in this module, along with the types from
"Common.Auth.Types".

It exports the 'AuthenticationAPI' type constructor, that returns a Servant API
type with endpoints for user login. Correspondingly, the @server@ exported
function takes an authentication function and builds a Servant server matching
the 'AuthenticationAPI'.

The 'AuthenticationAPI' requires an environment type that provides Servant
'Servant.Auth.Server.JWTSettings' and 'Servant.Auth.Server.CookieSettings', by
implementing 'Common.HasField.HasField'.

The 'AuthenticatedAPI' type constructor wraps a Servant API in a JWT
authentication combinator.

Both 'AuthenticationAPI' and 'AuthenticatedAPI' require a Servant context
that provide 'Servant.Auth.Server.JWTSettings' and
'Servant.Auth.Server.CookieSettings'. The exported 'apiContext' function can
build such a context, given an environment that provides these settings via
'Common.HasField.HasField'.
-}
module Common.Auth.HttpApi where

import Control.Monad.IO.Class
import Control.Effect.Reader
import Control.Effect.Error
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Servant hiding (throwError)
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Common.HasField
import Common.Auth.Types

{-|
  Constraints required for building the minimal Servant context with
  'apiContext'.
-}
type APIContextConstraints env = ( HasField "jwtSettings" env JWTSettings
                                 , HasField "cookieSettings" env CookieSettings
                                 )

{-|
  Type list of types required in the Servant context for Auth APIs.
-}
type APIContext = '[ JWTSettings
                   , CookieSettings
                   ]

{-|
  Wraps a Servant API in JWT authentication, expecting tokens that contain
  'AuthTokenClaims' with the specified #identity@ type.
-}
type AuthenticatedAPI identity api = Auth '[JWT] (AuthTokenClaims identity) :> api

{-|
  Builds a minimal Servant context for the APIs.
-}
apiContext :: APIContextConstraints env => env -> Context APIContext
apiContext env = #jwtSettings env
              :. #cookieSettings env
              :. EmptyContext

{-|
  Builds an Authentication API type with a single endpoint, which accept PUT or
  POST requests. These requests accept a JSON body that should be parsed into
  the @input@ type, and respond with a JSON body that contains the @identity@
  type, along with a JWT token.
-}
type AuthenticationAPI input identity = (
         ReqBody '[JSON] input :> Put '[JSON] (LoginReturn identity)
    :<|> ReqBody '[JSON] input :> Post '[JSON] (LoginReturn identity)
  )

{-|
  Constraints required to build an authentication server with 'server'.
-}
type ServerConstraints env sig m = ( Has (Error ServerError) sig m
                                   , MonadIO m
                                   , Has (Reader env) sig m
                                   , HasField "jwtSettings" env JWTSettings
                                   )

{-|
  Builds an authentication server that conforms to 'AuthenticationAPI'.
-}
server :: forall env sig m input identity. (ServerConstraints env sig m, ToJSON identity)
          => (env -> input -> m (Maybe identity))
          -> ServerT (AuthenticationAPI input identity) m
server authFn = login :<|> login
  where -- Handlers
    login input = do
      env <- ask
      maybeIdentity <- authFn env input
      identity <- case maybeIdentity of
                    Nothing -> throwError err401
                    Just identity -> return identity
      let claims = AuthTokenClaims $ Known identity
      jwtSettings <- asks @env #jwtSettings
      jwt <- liftIO $ makeJWT claims jwtSettings Nothing
      case jwt of
        Left _ -> throwError err500
        Right tok -> return $ LoginReturn identity (T.pack $ BS.unpack tok)
