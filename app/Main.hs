module Main where

import qualified AoC2022 (solve, day01_1, day01_2)

main :: IO ()
main = do
  putStrLn "Hello, Advent of Code 2022!"
--  solution <- (AoC2022.solve "inputs/day01.input" AoC2022.day01_1)  -- 69206
  solution <- (AoC2022.solve "inputs/day01.input" AoC2022.day01_2)  -- 197400
  putStrLn (show solution)
