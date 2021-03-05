module Control.Monad.Sample where

import Control.Monad.Probability
import qualified System.Random as R

class ProbabilityMonad m => SamplingMonad m where
  sample :: R.RandomGen g => m a -> g -> (a, g)