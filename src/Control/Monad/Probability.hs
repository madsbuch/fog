-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Probability
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module Control.Monad.Probability where

type Probability = Double -- number from 0 to 1

-- Probability Monad
class Monad m => ProbabilityMonad m where
  choose :: Double -> m a -> m a -> m a