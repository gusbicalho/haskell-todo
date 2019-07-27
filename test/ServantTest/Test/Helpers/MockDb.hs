module ServantTest.Test.Helpers.MockDb where

import Control.Monad.Writer
import Common.HasVal.Class
import ServantTest.Db.Transactor (Transactor(..))

data MockDb action = MockDb

data DbActions action a = DbActions [action] a deriving (Eq, Show)

type TestM action = Writer [[action]]

instance Transactor (MockDb action) (TestM action) (DbActions action) where
  transact :: MockDb action -> DbActions action a -> TestM action a
  transact _ (DbActions actions result) = tell [actions] >> return result

instance HasVal "transactor" (MockDb action) (MockDb action) where
  getVal = id

runTest :: Writer w a -> (a, w)
runTest = runWriter
