{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}

module Common.Config
  ( Config (..)
  , ConfigLoader
  , loadConfigFrom
  , defaultLoaders
  , loadConfig
  , loadFromEnv
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Aeson.Extra.Merge (lodashMerge)
import GHC.Exts
import Network.Wai.Handler.Warp (Port)
import Data.Either
import System.Environment (lookupEnv)

import Common.Version.Class (HasVersion(..))

data Config = Config { port :: Port
                     , version :: String
                     }
  deriving (Eq, Show)

instance HasVersion Config where
  getVersion = version

$(deriveJSON defaultOptions ''Config)

type ConfigLoader = IO (Either String Value)

waterfall :: [Either String Value] -> (Value, [String])
waterfall vs = (mergeAll (rights vs) (object []), lefts vs)
  where
    mergeAll []     acc = acc
    mergeAll (x:xs) acc = mergeAll xs (lodashMerge acc x)

loadConfigFrom :: [ConfigLoader] -> IO (Maybe Config)
loadConfigFrom configLoaders = do
    configs <- sequence configLoaders
    let (result, errors) = waterfall configs
    mapM_ printLoadError errors
    case parseEither parseJSON result of
      Left  err    -> return Nothing <* printLoadError err
      Right config -> return . Just $ config
  where
    printLoadError msg = putStrLn $ "[Load Config] Load failed: " ++ msg

defaultLoaders :: [ConfigLoader]
defaultLoaders = [loadFromEnv "SERVICE_CONFIG"]

loadConfig :: IO Config
loadConfig = loadConfigFrom defaultLoaders >>= orThrow
  where orThrow = maybe (ioError $ userError "[Load Config] Load failed.") return

loadFromEnv :: String -> ConfigLoader
loadFromEnv varName = do
  env <- lookupEnv varName
  case env of
    Nothing -> return . Left $ "Env var " ++ varName ++ " not found."
    Just envJson -> return . eitherDecode . fromString $ envJson
