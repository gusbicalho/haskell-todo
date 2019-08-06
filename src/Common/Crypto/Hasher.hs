module Common.Crypto.Hasher where

class Monad hashingM => Hasher t hashingM input hash | t input -> hash where
  maybeHash :: t -> input -> hashingM (Maybe hash)
  validate :: t -> hash -> input -> hashingM Bool
