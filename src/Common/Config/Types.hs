{-# LANGUAGE
    TemplateHaskell
  #-}

module Common.Config.Types
  ( Config (..)
  ) where

import Data.Aeson.TH
import Network.Wai.Handler.Warp (Port)

import Common.Version.Class (HasVersion(..))

data Config = Config { port :: Port
                     , version :: String
                     }
  deriving (Eq, Show)

instance HasVersion Config where
  getVersion = version

$(deriveJSON defaultOptions ''Config)
