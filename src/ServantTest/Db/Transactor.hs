module ServantTest.Db.Transactor where

class Transactor t transactionM statement | t -> statement where
  transact :: t -> statement a -> transactionM a

class HasTransactor p t | p -> t where
  getTransactor :: p -> t
