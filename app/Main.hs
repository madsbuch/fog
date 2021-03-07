module Main where

import DeathStats
import DeathData

main :: IO ()
main = do
  putStrLn "Objective: Verify following"
  putStrLn ""
  putStrLn "> The average Dane is expected to live seventy-seven years."
  putStrLn "> Therefore if you are seventy-two you can expect to live five more years,"
  putStrLn "> and should plan accordingly."
  putStrLn ""

  -- Our observations
  o <- observations
  let dist = deathRealm o

  putStr "Life Expectancy: "
  let a = expectedAge dist
  putStrLn $ show a

  putStr "Life Expectancy at 77: "
  let a = expectedAge $ deathRealmPerAge dist 77
  putStrLn $ show a
