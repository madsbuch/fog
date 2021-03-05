module ProbHelpers where

import Fog
import qualified System.Random as R

import Control.Monad.Probability
import Control.Monad.Sample

prob :: Bool -> Probability
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
nSamples n dist rGen =  let
                        (g1, g2) = R.split rGen
                      in
                        (sample dist g1) : (nSamples (n-1) dist g2)

quickSample :: Int -> P a -> [a]
quickSample num p = map (\(a, p) -> a) (nSamples num p (R.mkStdGen 42))