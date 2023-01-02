module Main where

import AoC2022
import qualified System.Exit as Exit
import Test.HUnit

dayTestCase :: (Show a, Eq a) => ([String] -> a) -> FilePath -> a -> Test
dayTestCase day input expectedResult = TestCase (do
  result <- solve input day
  assertEqual ("It's " ++ (show expectedResult)) expectedResult result)

labeledDayTestCase :: (Show a, Eq a) => String -> ([String] -> a) -> FilePath -> a -> Test
labeledDayTestCase name day input expectedResult = TestLabel name (dayTestCase day input expectedResult)

dayExampleTestCase :: (Show a, Eq a) => ([String] -> a) -> [String] -> a -> Test
dayExampleTestCase day input expectedResult = TestCase (do
  let result = day input
  assertEqual ("It's " ++ (show expectedResult)) expectedResult result)

labeledDayExampleTestCase :: (Show a, Eq a) => String -> ([String] -> a) -> [String] -> a -> Test
labeledDayExampleTestCase name day input expectedResult = TestLabel name (dayExampleTestCase day input expectedResult)

tests :: Test
tests = TestList [
    labeledDayTestCase "day 01 part 1" day01_1 "inputs/day01.input" 69206
   ,labeledDayTestCase "day 01 part 2" day01_2 "inputs/day01.input" 197400
   ,labeledDayTestCase "day 02 part 1" day02_1 "inputs/day02.input" 12458
   ,labeledDayTestCase "day 02 part 2" day02_2 "inputs/day02.input" 12683
   ,labeledDayTestCase "day 03 part 1" day03_1 "inputs/day03.input" 8252
   ,labeledDayTestCase "day 03 part 2" day03_2 "inputs/day03.input" 2828
   ,labeledDayTestCase "day 04 part 1" day04_1 "inputs/day04.input" 542
   ,labeledDayTestCase "day 04 part 2" day04_2 "inputs/day04.input" 900
   ,labeledDayTestCase "day 05 part 1" day05_1 "inputs/day05.input" "BWNCQRMDB"
   ,labeledDayTestCase "day 05 part 2" day05_2 "inputs/day05.input" "NHWZCBNBF"
   ,labeledDayTestCase "day 06 part 1" day06_1 "inputs/day06.input" 0
   ,labeledDayTestCase "day 06 part 2" day06_2 "inputs/day06.input" 0
   ,labeledDayExampleTestCase "day 06 part 1 example 1" day06_1 ["bvwbjplbgvbhsrlpgdmjqwftvncz"] 5
   ,labeledDayExampleTestCase "day 06 part 1 example 2" day06_1 ["nppdvjthqldpwncqszvftbrmjlhg"] 6
   ,labeledDayExampleTestCase "day 06 part 1 example 3" day06_1 ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"] 10
   ,labeledDayExampleTestCase "day 06 part 1 example 4" day06_1 ["zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"] 11
  ]

main :: IO Counts
main = do
    count <- runTestTT tests
    if failures count > 0 || errors count > 0 then Exit.exitFailure else return count
