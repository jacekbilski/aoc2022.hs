module AoC2022 (solve, readInput, day01_1, day01_2, day02_1, day02_2, day03_1, day03_2, day04_1, day04_2, day05_1, day05_2,
  day06_1, day06_2, day07_1, day07_2, day08_1, day08_2, day09_1, day09_2, day10_1, day10_2, day11_1, day11_2,
  day12_1, day12_2, day13_1, day13_2, day14_1, day14_2, day15_1, day15_2, day16_1, day16_2) where

import System.IO

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16

solve :: FilePath -> ([String] -> a) -> IO a
solve fileName day = do
  input <- readInput fileName
  return (day input)

readInput :: FilePath -> IO [String]
readInput fileName = do
  input <- withFile fileName ReadMode hGetContents'
  return (lines input)
