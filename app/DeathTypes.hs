module DeathTypes where

data Gender = Men | Women deriving (Show, Ord, Eq)
type Age = Double
type Year = Int
type NumObservations = Int
data Nationality = Danish
type Observation = (Age, Gender, Nationality, Year)
type ObservationWithCount = (Age, Gender, Nationality, Year, NumObservations)