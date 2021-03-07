-----------------------------------------------------------------------------
-- |
-- Module      :  MeasureTerms
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module MeasureTerms where

import Control.Monad (liftM, ap)
import Control.Monad.Probability
import Control.Monad.Support
import Control.Monad.Sample
import Control.Monad.Expectation

-- Probability Monad Type
-- This representation is effective to calculate expectation.
newtype PExp a = PExp (( a -> Double) -> Double)

-- PExp needs to be a functor to be a monad
instance Functor PExp where
  fmap = liftM

-- PExp needs to be an applicative to be a monad
instance Applicative PExp where
  pure  = return
  (<*>) = ap

-- PExp is a monad
instance Monad PExp where
  return x = PExp (\h -> h x)
  (PExp d) >>= k =
       PExp (\h -> let
                  apply (PExp f) arg = f arg
                  g x             = apply (k x) h
                in
                  d g )

-- PExp is a probability monad
instance ProbabilityMonad PExp where
  choose p (PExp d1) (PExp d2) =
        PExp (\h -> p * d1 h + (1 - p) * d2 h)

-- Not easily implemented
instance SupportMonad PExp where
  support (PExp h) = undefined

-- Easily implemented! 
instance ExpectationMonad PExp where
  expectation h (PExp d) = d h

-- Not easily implemented
instance SamplingMonad PExp where
  sample = undefined