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
import Data.Foldable (foldl')
import qualified Data.Text as T
import System.Environment (lookupEnv)

type ConfigLoader = IO (Either T.Text Value)

waterfall :: [Either T.Text Value] -> (Value, [T.Text])
waterfall vs = (mergeAll (rights vs) (object []), lefts vs)
  where
    mergeAll xs acc = foldl' lodashMerge acc xs

loadConfigFrom :: FromJSON a => [ConfigLoader] -> IO (Maybe a)
loadConfigFrom configLoaders = do
    configs <- sequence configLoaders
    let (result, errors) = waterfall configs
    mapM_ (printLoadError . T.unpack) errors
    case parseEither parseJSON result of
      Left  err    -> Nothing <$ printLoadError err
      Right config -> return . Just $ config
  where
    printLoadError msg = putStrLn $ "[Load Config] Load failed: " ++ msg

defaultLoaders :: [ConfigLoader]
defaultLoaders = [loadFromEnv "SERVICE_CONFIG"]

loadConfig :: FromJSON a => IO a
loadConfig = loadConfigFrom defaultLoaders >>= orThrow
  where orThrow = maybe (ioError $ userError "[Load Config] Load failed.") return

loadFromEnv :: T.Text -> ConfigLoader
loadFromEnv varName = do
    env <- lookupEnv $ T.unpack varName
    case env of
      Nothing -> return . Left $ T.concat ["Env var ", varName, " not found."]
      Just envJson -> return . leftToText . eitherDecode . fromString $ envJson
  where leftToText (Right v)  = Right v
        leftToText (Left str) = Left $ T.pack str
