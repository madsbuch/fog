module Main where

import DeathStats
import DeathData

main :: IO ()
main = do
  putStrLn "Objective: Verify following"
  putStrLn "The average American is expected to live seventy-three years."
  putStrLn "Therefore if you are sixty-eight you can expect to live five more years,"
  putStrLn "and should plan accordingly."
  o <- observations
  let a = expectedAge $ deathRealm o
  putStrLn $ show a
