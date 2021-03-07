-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Sample
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module Control.Monad.Sample where

import Control.Monad.Probability
import qualified System.Random as R

class ProbabilityMonad m => SamplingMonad m where
  sample :: R.RandomGen g => m a -> g -> (a, g)