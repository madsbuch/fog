module Control.Monad.Expectation where

import Control.Monad.Probability

-- Expectation Monad
class ProbabilityMonad m => ExpectationMonad m where
  expectation :: (a -> Double) -> m a -> Double