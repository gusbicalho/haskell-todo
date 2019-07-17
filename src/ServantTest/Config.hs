module ServantTest.Config
  ( Config (..)
  ) where

import Network.Wai.Handler.Warp (Port)

import Version.Class (HasVersion(..))

data Config = Config { port :: Port
                     , version :: String
                     }

instance HasVersion Config where
  getVersion = version
