{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Effect.JWT where

import           Control.Carrier
import           Control.Monad.IO.Class
import qualified Servant.Auth.Server as SAS (makeJWT, JWTSettings, ToJWT)
import qualified Crypto.JOSE as JOSE
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time (UTCTime)

data JWTOps m k =
  forall claims . SAS.ToJWT claims
  => MakeJWT claims SAS.JWTSettings (Maybe UTCTime) (Either JOSE.Error Text -> m k)
  -- deriving (Generic1, Functor, HFunctor, Effect)

instance Functor m => Functor (JWTOps m) where
  fmap f (MakeJWT c s t k) = MakeJWT c s t (\r -> f <$> k r)

instance HFunctor JWTOps where
  hmap f (MakeJWT c s t k) = MakeJWT c s t (f . k)

instance Effect JWTOps where
  handle state handler (MakeJWT c s t k) = MakeJWT c s t (\r -> handler (k r <$ state))

makeJWT :: ( Has JWTOps sig m
           , SAS.ToJWT claims
           ) => claims -> SAS.JWTSettings -> Maybe UTCTime -> m (Either JOSE.Error Text)
makeJWT claims jwtSettings maybeTime = send (MakeJWT claims jwtSettings maybeTime pure)

newtype JWTOpsIOC (m :: * -> *) a = JWTOpsIOC { runJWTOps :: (m a) }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ( Carrier sig m
         , Effect sig
         , MonadIO m
         ) => Carrier (JWTOps :+: sig) (JWTOpsIOC m) where
  eff (L (MakeJWT claims jwtSettings maybeTime k)) = do
    jwt <- liftIO $ SAS.makeJWT claims jwtSettings maybeTime
    k $ T.pack . BS.unpack <$> jwt
  eff (R other)            = send other
  {-# INLINE eff #-}
