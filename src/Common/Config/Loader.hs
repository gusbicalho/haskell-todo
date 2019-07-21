module Common.Config.Loader
  ( ConfigLoader
  , loadConfigFrom
  , defaultLoaders
  , loadConfig
  , loadFromEnv
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Extra.Merge (lodashMerge)
import GHC.Exts
import Data.Either
import System.Environment (lookupEnv)

type ConfigLoader = IO (Either String Value)

waterfall :: [Either String Value] -> (Value, [String])
waterfall vs = (mergeAll (rights vs) (object []), lefts vs)
  where
    mergeAll []     acc = acc
    mergeAll (x:xs) acc = mergeAll xs (lodashMerge acc x)

loadConfigFrom :: FromJSON a => [ConfigLoader] -> IO (Maybe a)
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

loadConfig :: FromJSON a => IO a
loadConfig = loadConfigFrom defaultLoaders >>= orThrow
  where orThrow = maybe (ioError $ userError "[Load Config] Load failed.") return

loadFromEnv :: String -> ConfigLoader
loadFromEnv varName = do
  env <- lookupEnv varName
  case env of
    Nothing -> return . Left $ "Env var " ++ varName ++ " not found."
    Just envJson -> return . eitherDecode . fromString $ envJson
