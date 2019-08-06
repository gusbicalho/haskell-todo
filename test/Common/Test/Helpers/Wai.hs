module Common.Test.Helpers.Wai where

import Test.Hspec.Wai

import Data.ByteString as BS
import Data.ByteString.Lazy as BS.Lazy
import Network.Wai.Test (SResponse)
import Network.HTTP.Types

postJson :: BS.ByteString
            -> BS.Lazy.ByteString
            -> WaiSession SResponse
postJson path = request methodPost path [(hContentType, "application/json")]

putJson :: BS.ByteString
           -> BS.Lazy.ByteString
           -> WaiSession SResponse
putJson path = request methodPut path [(hContentType, "application/json")]
