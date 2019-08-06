module ServantTest.Test.Helpers.MockEnv where

import Control.Monad.Writer
import Data.Text as T
import Common.HasVal.Class
import Common.Db.Transactor
import Common.Crypto.Hasher

data MockEnv dbAction = MockEnv

-- Transactor
data MockDb dbAction = MockDb
instance HasVal "transactor" (MockEnv action) (MockDb action) where
  getVal _ = MockDb

data DbActions action a = DbActions [action] a deriving (Eq, Show)

instance Functor (DbActions action) where
  fmap f (DbActions actions a) = DbActions actions (f a)

instance Applicative (DbActions action) where
  pure a = DbActions [] a
  (DbActions actionsF f) <*> (DbActions actionsA a) = DbActions (actionsF <> actionsA) (f a)

instance Monad (DbActions action) where
  return a = DbActions [] a
  (DbActions actions a) >>= f = let DbActions moreActions b = f a
                                in DbActions (actions <> moreActions) b

type TestM action = Writer [[action]]

instance Transactor (MockDb action) (TestM action) (DbActions action) where
  transact :: MockDb action -> DbActions action a -> TestM action a
  transact _ (DbActions actions result) = tell [actions] >> return result

runTest :: Writer w a -> (a, w)
runTest = runWriter

-- Hasher
data MockCrypto = MockCrypto
instance HasVal "hasher" (MockEnv action) MockCrypto where
  getVal _ = MockCrypto

instance Monad m => Hasher MockCrypto m T.Text T.Text where
  maybeHash _ input = return $ Just $ "hash_" <> input
  validate _ hash input = return $ hash == "hash_" <> input
