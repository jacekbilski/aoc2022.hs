module Main where

import AoC2022
import qualified System.Exit as Exit
import Test.HUnit

dayTestCase :: ([String] -> Int) -> FilePath -> Int -> Test
dayTestCase day input expectedResult = TestCase (do
  result <- solve input day
  assertEqual ("It's " ++ (show expectedResult)) expectedResult result)

labeledDayTestCase :: String -> ([String] -> Int) -> FilePath -> Int -> Test
labeledDayTestCase name day input expectedResult = TestLabel name (dayTestCase day input expectedResult)

tests :: Test
tests = TestList [
    labeledDayTestCase "day 01 part 1" day01_1 "inputs/day01.input" 69206
   ,labeledDayTestCase "day 01 part 2" day01_2 "inputs/day01.input" 197400
   ,labeledDayTestCase "day 02 part 1" day02_1 "inputs/day02.input" 12458
   ,labeledDayTestCase "day 02 part 2" day02_2 "inputs/day02.input" 12683
   ,labeledDayTestCase "day 03 part 1" day03_1 "inputs/day03.input" 8252
   ,labeledDayTestCase "day 03 part 2" day03_2 "inputs/day03.input" 2828
   ,labeledDayTestCase "day 04 part 1" day04_1 "inputs/day04.input" 542
   ,labeledDayTestCase "day 04 part 2" day04_2 "inputs/day04.input" (-1)
  ]

main :: IO Counts
main = do
    count <- runTestTT tests
    if failures count > 0 || errors count > 0 then Exit.exitFailure else return count
