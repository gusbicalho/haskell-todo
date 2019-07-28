module ServantTest.Test.Helpers.Wai where

import Test.Hspec.Wai

import Network.Wai
import Data.ByteString as BS
import Data.ByteString.Lazy as BS.Lazy
import Network.Wai.Test (SResponse)
import Network.HTTP.Types

postJson :: BS.ByteString
            -> BS.Lazy.ByteString
            -> WaiSession SResponse
postJson path body = request methodPost path [(hContentType, "application/json")] body
