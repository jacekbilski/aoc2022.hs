module AoC2022 (solve, day01_1, day01_2, day02_1, day02_2, day03_1, day03_2, day04_1, day04_2, day05_1, day05_2, day06_1, day06_2) where

import Flow
import System.IO

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

solve :: FilePath -> ([String] -> a) -> IO a
solve fileName day = do
  input <- readInput fileName
  return (lines input |> day)

readInput :: FilePath -> IO String
readInput fileName = withFile fileName ReadMode hGetContents'
