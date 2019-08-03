module ServantTest.Db.Transactor where

class (Monad action, Monad transactionM) => Transactor t transactionM action | t -> action where
  transact :: t -> action a -> transactionM a
