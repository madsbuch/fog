module Control.Monad.Probability where

type Probability = Double -- number from 0 to 1

-- Probability Monad
class Monad m => ProbabilityMonad m where
  choose :: Probability -> m a -> m a -> m a