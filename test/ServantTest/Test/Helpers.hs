module ServantTest.Test.Helpers where

import Control.Monad.Writer
import ServantTest.Db.Transactor (Transactor(..), HasTransactor(..))

data MockDb = MockDb
data DbAction = DbAction String deriving (Eq, Show)
data DbActions a = DbActions [DbAction] a deriving (Eq, Show)

type TestM = Writer [[DbAction]]

instance Transactor MockDb TestM DbActions where
  transact :: MockDb -> DbActions a -> TestM a
  transact _ (DbActions actions result) = tell [actions] >> return result

instance HasTransactor MockDb MockDb where
  getTransactor = id

runTest = runWriter
