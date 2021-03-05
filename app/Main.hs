module Main where

import DeathStats
import DeathData

main :: IO ()
main = do
  putStrLn "Question: Expected Age"
  o <- observations
  let a = expectedAge $ deathRealm o
  putStrLn $ show a
