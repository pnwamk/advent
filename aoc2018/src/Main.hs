module Main where

import qualified Day01 as Day01
import qualified Day02 as Day02
import qualified Day03 as Day03
import qualified Day04 as Day04
import qualified Day05 as Day05

main :: IO ()
main = do
  -- Day 1
  putStrLn "Day01"
  Day01.test
  Day01.run
  -- Day 2
  putStrLn "Day02"
  Day02.test
  Day02.run
  -- Day 3
  putStrLn "Day03"
  Day03.test
  Day03.run
  -- Day 4
  putStrLn "Day04"
  Day04.test
  Day04.run
  -- Day 5
  putStrLn "Day05"
  Day05.test
  Day05.run
