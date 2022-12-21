module AoC2022 (solve, day01_1, day01_2, day02_1, day02_2, day03_1, day03_2, day04_1, day04_2, day05_1, day05_2) where

import Control.Exception
import Flow
import System.IO

import Day01
import Day02
import Day03
import Day04
import Day05

solve :: (Show a) => FilePath -> ([String] -> a) -> IO a
solve fileName day = do
  input <- (readInput fileName)
  return (lines input |> day)

readInput :: FilePath -> IO String
readInput fileName = bracket (openFile fileName ReadMode) hClose hGetContents'
