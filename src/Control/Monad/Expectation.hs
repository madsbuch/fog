-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Expectation
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module Control.Monad.Expectation where

import Control.Monad.Probability

-- Expectation Monad
class ProbabilityMonad m => ExpectationMonad m where
  expectation :: (a -> Double) -> m a -> Double