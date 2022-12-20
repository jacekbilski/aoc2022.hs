module Main where

import Test.HUnit
import qualified AoC2022 (solve, day01_1, day01_2)

test_day01_1 :: Test
test_day01_1 = TestCase (do
  result <- AoC2022.solve "inputs/day01.input" AoC2022.day01_1
  assertEqual "It's 69206" 69216 result)

test_day01_2 :: Test
test_day01_2 = TestCase (do
  result <- AoC2022.solve "inputs/day01.input" AoC2022.day01_2
  assertEqual "It's 197400" 197401 result)

tests :: Test
tests = TestList [TestLabel "day 01 part 1" test_day01_1, TestLabel "day 01 part 2" test_day01_2]

main :: IO Counts
main = runTestTT tests
