module ServantTest.Test.Helpers where

import Control.Monad.Writer
import ServantTest.Db.Transactor (Transactor(..), HasTransactor(..))

data MockDb action = MockDb

data DbActions action a = DbActions [action] a deriving (Eq, Show)

type TestM action = Writer [[action]]

instance Transactor (MockDb action) (TestM action) (DbActions action) where
  transact :: MockDb action -> DbActions action a -> TestM action a
  transact _ (DbActions actions result) = tell [actions] >> return result

instance HasTransactor (MockDb action) (MockDb action) where
  getTransactor = id

runTest :: Writer w a -> (a, w)
runTest = runWriter
