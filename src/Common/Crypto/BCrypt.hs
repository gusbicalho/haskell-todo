{-|
Description: Implementation of Hasher using BCrypt
-}
module Common.Crypto.BCrypt where

import Control.Monad.IO.Class
import Crypto.BCrypt
import Common.Crypto.Hasher
import Data.ByteString.Char8 as B8
import Data.Text as T

newtype BCrypter = BCrypter { bcryptPolicy :: HashingPolicy }

instance MonadIO m => Hasher BCrypter m B8.ByteString B8.ByteString where
  maybeHash (BCrypter policy) input = liftIO $ hashPasswordUsingPolicy policy input
  validate _ hash input = return $ validatePassword hash input

textToBS :: T.Text -> B8.ByteString
textToBS = B8.pack . T.unpack

bsToText :: B8.ByteString -> T.Text
bsToText = T.pack . B8.unpack

instance MonadIO m => Hasher BCrypter m T.Text T.Text where
  maybeHash (BCrypter policy) input = liftIO $ do
    maybeHashed <- hashPasswordUsingPolicy policy (textToBS input)
    return $ bsToText <$> maybeHashed
  validate _ hash input = return $ validatePassword (textToBS hash) (textToBS input)
