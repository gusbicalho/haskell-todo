module Common.Db.Transactor where

class Monad transactionM => Transactor t transactionM action | t -> action where
  transact :: t -> action a -> transactionM a