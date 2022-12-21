module Main where

import Test.HUnit
import AoC2022
import qualified System.Exit as Exit

test_day01_1 :: Test
test_day01_1 = TestCase (do
  result <- solve "inputs/day01.input" day01_1
  assertEqual "It's 69206" 69206 result)

test_day01_2 :: Test
test_day01_2 = TestCase (do
  result <- solve "inputs/day01.input" day01_2
  assertEqual "It's 197400" 197400 result)

test_day02_1 :: Test
test_day02_1 = TestCase (do
  result <- solve "inputs/day02.input" day02_1
  assertEqual "It's 12458" 12458 result)

test_day02_2 :: Test
test_day02_2 = TestCase (do
  result <- solve "inputs/day02.input" day02_2
  assertEqual "It's 12683" 12683 result)

test_day03_1 :: Test
test_day03_1 = TestCase (do
  result <- solve "inputs/day03.input" day03_1
  assertEqual "It's 8252" 8252 result)

test_day03_2 :: Test
test_day03_2 = TestCase (do
  result <- solve "inputs/day03.input" day03_2
  assertEqual "It's >" (-1) result)

tests :: Test
tests = TestList [
    TestLabel "day 01 part 1" test_day01_1
   ,TestLabel "day 01 part 2" test_day01_2
   ,TestLabel "day 02 part 1" test_day02_1
   ,TestLabel "day 02 part 2" test_day02_2
   ,TestLabel "day 03 part 1" test_day03_1
   ,TestLabel "day 03 part 2" test_day03_2
  ]

main :: IO Counts
main = do
    count <- runTestTT tests
    if failures count > 0 || errors count > 0 then Exit.exitFailure else return count
