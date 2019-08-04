{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Redundant do" -}
module Common.Auth.HttpApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Data.Aeson hiding (json)
import GHC.Generics
import Network.Wai
import Servant
import Servant.Auth.Server as SAS
import Control.Monad.Reader

import Common.HasVal.Class
import Common.Auth.HttpApi as HttpApi
import Common.Test.Helpers.Wai

data MockEnv = MockEnv JWTSettings
instance HasVal "cookieSettings" MockEnv CookieSettings where
  getVal _ = defaultCookieSettings
instance HasVal "jwtSettings" MockEnv JWTSettings where
  getVal (MockEnv jwtSettings) = jwtSettings

data Input = Input { username :: String, password :: String } deriving (Eq, Show, Generic)
instance FromJSON Input where
data Identity = Identity { username :: String, id :: Integer } deriving (Eq, Show, Generic)
instance ToJSON Identity where

api :: Proxy (HttpApi.AuthenticationAPI Input Identity)
api = Proxy

app :: IO Application
app = do
    jwk <- readKey "resources/test/test.key"
    let env = MockEnv (defaultJWTSettings jwk)
        hoisted = hoistServer api (provideDependencies env) (HttpApi.server authorize)
    return $ serve api hoisted
  where
    provideDependencies env m = runReaderT m env
    authorize _ (Input "user" "pass") = return . Just $ Identity "user" 1
    authorize _ _                     = return Nothing


spec :: Spec
spec = beforeAll app $ do
  describe "user can login with username and password" $ do
    it "POST / responds with identity and token" $
      postJson "/" [json|{username: "user", password: "pass"}|]
        `shouldRespondWith` [json|{ identity: {id: 1, username: "user"}
                                  , token: "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiaWRlbnRpdHkiOnsidXNlcm5hbWUiOiJ1c2VyIiwiaWQiOjF9fX0.mXV3zPFfFo3AHK5FWKurQ0eMFCmnmdpJ5154GA6SEpWJODMohhDUImQI2IceKS4PC6_XxdK4ILUDG5QzJ1Or4Q"
                                  }|]
    it "PUT / responds with identity and token" $
      putJson "/" [json|{username: "user", password: "pass"}|]
        `shouldRespondWith` [json|{ identity: {id: 1, username: "user"}
                                  , token: "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiaWRlbnRpdHkiOnsidXNlcm5hbWUiOiJ1c2VyIiwiaWQiOjF9fX0.mXV3zPFfFo3AHK5FWKurQ0eMFCmnmdpJ5154GA6SEpWJODMohhDUImQI2IceKS4PC6_XxdK4ILUDG5QzJ1Or4Q"
                                  }|]
  describe "wrong login data" $ do
    it "POST / responds with 401" $
      postJson "/" [json|{username: "foo", password: "bar"}|]
        `shouldRespondWith` 401
    it "PUT / responds with 401" $
      putJson "/" [json|{username: "foo", password: "bar"}|]
        `shouldRespondWith` 401
