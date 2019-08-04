{- HLINT ignore "Redundant do" -}
module Common.Config.LoaderSpec (spec) where

import Test.Hspec
import qualified Common.Config.Loader as Loader
import Control.Exception
import Data.Aeson as AE
import GHC.Generics
import System.Environment (lookupEnv, setEnv, unsetEnv)

withEnvSet :: String -> Maybe String -> IO a -> IO a
withEnvSet envName newVal action = do
    oldVal <- lookupEnv envName
    resetEnv newVal
    action `finally` resetEnv oldVal
  where
    resetEnv Nothing = unsetEnv envName
    resetEnv (Just oldVal) = setEnv envName oldVal

spec :: Spec
spec = do
  describe "loadFromFile" $ do
    it "loads a file and parses it into json" $
      Loader.loadFromFile "resources/test/partial_config.json"
      `shouldReturn` Right (AE.object [("foo", AE.Number 27)])
    it "return an error message when trying to load a missing file" $
      Loader.loadFromFile "resources/test/missing.json"
      `shouldReturn` Left "Loading file resources/test/missing.json: resources/test/missing.json: openBinaryFile: does not exist (No such file or directory)"
    it "return an error message when trying to load a malformed file" $
      Loader.loadFromFile "resources/test/malformed.json"
      `shouldReturn` Left "Loading file resources/test/malformed.json: Error in $: Failed reading: not a valid json value"
  describe "loadFromEnv" $ do
    it "loads from json string in an env var" $
      withEnvSet "foobar" (Just "{ \"bar\": 42 }") (Loader.loadFromEnv "foobar")
      `shouldReturn` Right (AE.object [("bar", AE.Number 42)])
    it "returns an error message for a missing env var" $
      withEnvSet "foobar" Nothing (Loader.loadFromEnv "foobar")
      `shouldReturn` Left "Loading env foobar: Env var foobar not found."
    it "returns an error message for a malformed env var" $
      withEnvSet "foobar" (Just "{ \"bar\": }") (Loader.loadFromEnv "foobar")
      `shouldReturn` Left "Loading env foobar: Error in $: Failed reading: not a valid json value"
  describe "loadConfigFrom" $ do
    it "should run several loaders, ignore errors, and merge results, with later loaders overriding previous ones" $
      Loader.loadConfigFrom @FooConfig [ mockLoader1
                                       , mockLoader2
                                       , mockLoader3
                                       ]
      `shouldReturn` Just (FooConfig { foo = 27
                                     , bar = False
                                     , baz = Nothing
                                     , bop = Just "bebop"
                                     })
    it "should return Nothing if we cannot build a full config" $
      Loader.loadConfigFrom @FooConfig [mockLoader2]
      `shouldReturn` Nothing

data FooConfig = FooConfig {
  foo :: Integer
, bar :: Bool
, baz :: Maybe String
, bop :: Maybe String
} deriving (Eq, Show, Generic)
instance FromJSON FooConfig where
instance ToJSON FooConfig where

mockLoader1 :: Loader.ConfigLoader
mockLoader1 = return . Right . AE.object $
               [("foo", AE.Number 42)
               ,("bar", AE.Bool True)
               ,("baz", AE.String "Bazzz")
               ]

mockLoader2 :: Loader.ConfigLoader
mockLoader2 = return . Right . AE.object $
               [("bar", AE.Bool False)
               ,("bop", AE.String "bebop")
               ]

mockLoader3 :: Loader.ConfigLoader
mockLoader3 = return . Right . AE.object $
               [("foo", AE.Number 27)
               ,("baz", AE.Null)
               ]
