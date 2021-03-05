module DeathStats where

import Fog
import ProbHelpers
import Control.Monad.Probability
import qualified System.Random as R
import DeathTypes


expectationBySampling :: (Ord a) => (a -> Double) -> P a -> Double
expectationBySampling f p = let
  (rCount, rSum) = innerF $ quickSample 10000 p
  in
    rSum / rCount
  where
    innerF [s] = (f s, 1.0)
    innerF (s : ss) = let
      (rCount, rSum) = innerF ss
      sum = f s
      in
        (rCount + 1, rSum + sum)

expectedAge :: P Observation -> Age
expectedAge p = let
  (sum, num) = innerF $ quickSample 10000 p
  in
    sum / num
  where
    innerF [] = (0.0, 0.0)
    innerF ((a, _, _, _) : rest) = let
      (sum, num) = innerF rest
      in
        (sum + a, num + 1.0)

-- Distributions

-- Simply give us a death person
deathRealmRaw :: [ObservationWithCount] -> P Observation
deathRealmRaw observations = uniform $ explode observations

deathRealm :: [ObservationWithCount] -> P Observation
deathRealm observations = dr observations
  where
    dr :: [ObservationWithCount] -> P Observation
    dr [(a, g, n, y, _)] = return (a, g, n, y)
    dr l@((a, g, n, y, num) : rest) = do 
      let sum = sumCount l
      choose ((fromIntegral num) / (fromIntegral sum)) (return (a, g, n, y)) (dr rest)

deathRealmPerAge :: P Observation -> Age -> P Observation
deathRealmPerAge deathRealm age = do
  (sampledAge, gender, n, y) <- deathRealm
  if sampledAge >= age
    then return (sampledAge, gender, n, y)
    else deathRealmPerAge deathRealm age

deathRealmGender :: P Observation -> P Gender
deathRealmGender deathRealm = do
  (_, gender, _, _) <- deathRealm
  return gender

explode [] = []
explode ((_, _, _, _, 0) : rest) = explode rest
explode ((a, g, n, y, num) : rest) = (a, g, n, y) : (explode ((a, g, n, y, num - 1) : rest))

sumCount [] = 0
sumCount ((_, _, _, _, n) : rest) = n + (sumCount rest)

