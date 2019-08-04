module ServantTest.Test.Helpers.MockDb where

import Control.Monad.Writer
import Common.HasVal.Class
import Common.Db.Transactor (Transactor(..))

data MockDb action = MockDb

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

instance HasVal "transactor" (MockDb action) (MockDb action) where
  getVal = id

runTest :: Writer w a -> (a, w)
runTest = runWriter
