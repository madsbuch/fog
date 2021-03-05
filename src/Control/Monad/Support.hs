module Control.Monad.Support where

import Control.Monad.Probability

-- Support Monad
class ProbabilityMonad m => SupportMonad m where
  support :: m a -> [a]