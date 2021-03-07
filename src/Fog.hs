-----------------------------------------------------------------------------
-- |
-- Module      :  Fog
-- Copyright   :  (c) Mads Buch 2021
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  me@madsbuch.com
-- Stability   :  experimental
--
-- See clear in the fog
--
-----------------------------------------------------------------------------

module Fog (module Data.P, quickSample, uniform) where

import Data.List
import qualified System.Random as R
import Control.Applicative
import Control.Monad (liftM, ap)

import Control.Monad.Probability
import Control.Monad.Support
import Control.Monad.Sample
import Control.Monad.Expectation
import Data.P

prob :: Bool -> Double
prob b = if b then 1 else 0

-- Create a uniform distribution over a list of elements
uniform :: [a] -> P a
uniform [x] = return x
uniform ls@(x:xs) =
        let p = 1.0 / ( fromIntegral (length ls) )
        in choose p (return x) (uniform xs)

-- taking samples
nSamples :: R.RandomGen g => Int -> P a -> g -> [(a, g)]
nSamples 0 dist gen = []
nSamples n dist rGen = let
                        (g1, g2) = R.split rGen
                      in
                        (sample dist g1) : (nSamples (n-1) dist g2)

quickSample :: Int -> P a -> [a]
quickSample num p = map (\(a, p) -> a) (nSamples num p (R.mkStdGen 42))