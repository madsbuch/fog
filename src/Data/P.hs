{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.P
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module Data.P where

import qualified System.Random as R
import Control.Monad (liftM, ap)
import Control.Monad.Probability
import Control.Monad.Support
import Control.Monad.Sample
import Control.Monad.Expectation

{-- The general probability monad --}
data P a where
    -- Return
    R :: a -> P a 
    -- Bind,  The reason for GADT
    B :: P a -> (a -> P b) -> P b
    -- Choose
    C :: Double -> P a -> P a -> P a

-- P needs to be a functor to be a monad
instance Functor P where
  fmap = liftM

-- P needs to be an applicative to be a monad
instance Applicative P where
  pure  = return
  (<*>) = ap

-- P is a monad
instance Monad P where
  return x = R x
  d >>= k  = B d k

-- P is a probability monad
instance ProbabilityMonad P where
  choose p d1 d2 = C p d1 d2

instance SupportMonad P where
  support (R x) = [x]
  support (B d k) = concat [support (k x) | x <- support d]
  support (C p d1 d2) = support d1 ++ support d2

instance ExpectationMonad P where
  expectation h (R x) = h x
  expectation h (B d k) = expectation g d
    where
      g x = expectation h (k x)
  expectation h (C p d1 d2) =
         (p  * expectation h d1)
    + ((1-p) * expectation h d2)

instance SamplingMonad P where
  sample (R x) g = (x, g)
  sample (B d k) g =  let
                        (x, g') = sample d g
                      in
                        sample (k x) g'
  sample (C p d1 d2) g = let
                            (x, g') = R.random g
                          in 
                            sample (if x < p then d1 else d2) g'