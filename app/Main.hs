module Main where

import qualified AoC2022 (solve, day01_1, day01_2)

main :: IO ()
main = do
  putStrLn "Hello, Advent of Code 2022!"
--  AoC2022.solve AoC2022.day01_1 "inputs/day01.input"  -- 69206
  AoC2022.solve AoC2022.day01_2 "inputs/day01.input"  -- 197400
