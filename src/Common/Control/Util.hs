module Common.Control.Util where

andThen :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
f `andThen` g = \a -> f a >> g a
