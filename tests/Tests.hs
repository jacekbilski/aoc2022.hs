module Tests where

import AoC2022
import Test.Tasty
import Test.Tasty.HUnit

type Solution a = [String] -> a
type Sample a = [String] -> a

dayTestCase :: (Show a, Eq a) => Solution a -> FilePath -> a -> Assertion
dayTestCase day input expectedResult = do
  result <- solve input day
  assertEqual ("It's " ++ show expectedResult) expectedResult result

labeledDayTestCase :: (Show a, Eq a) => String -> Solution a -> FilePath -> a -> TestTree
labeledDayTestCase name day input expectedResult = testCase name (dayTestCase day input expectedResult)

dayExampleTestCase :: (Show a, Eq a) => Sample a -> [String] -> a -> Assertion
dayExampleTestCase day input expectedResult = do
--  result <- day input
  let result = day input
  assertEqual ("It's " ++ show expectedResult) expectedResult result

labeledDayExampleTestCase :: (Show a, Eq a) => String -> Sample a -> [String] -> a -> TestTree
labeledDayExampleTestCase name day input expectedResult = testCase name (dayExampleTestCase day input expectedResult)

test_aoc :: TestTree
test_aoc = testGroup "Advent of Code 2022" [
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
   ,labeledDayTestCase "day 06 part 1" day06_1 "inputs/day06.input" 1109
   ,labeledDayTestCase "day 06 part 2" day06_2 "inputs/day06.input" 3965
   ,labeledDayTestCase "day 07 part 1" day07_1 "inputs/day07.input" 1783610
   ,labeledDayTestCase "day 07 part 2" day07_2 "inputs/day07.input" 4370655
   ,labeledDayTestCase "day 08 part 1" day08_1 "inputs/day08.input" 1798
   ,labeledDayTestCase "day 08 part 2" day08_2 "inputs/day08.input" 259308
   ,labeledDayTestCase "day 09 part 1" day09_1 "inputs/day09.input" 6357
   ,labeledDayTestCase "day 09 part 2" day09_2 "inputs/day09.input" 2627
   ,labeledDayTestCase "day 10 part 1" day10_1 "inputs/day10.input" 15360
   ,labeledDayTestCase "day 10 part 2" day10_2 "inputs/day10.input" [
    "###..#..#.#....#..#...##..##..####..##..",
    "#..#.#..#.#....#..#....#.#..#....#.#..#.",
    "#..#.####.#....####....#.#......#..#..#.",
    "###..#..#.#....#..#....#.#.##..#...####.",
    "#....#..#.#....#..#.#..#.#..#.#....#..#.",
    "#....#..#.####.#..#..##...###.####.#..#."]
   ,labeledDayTestCase "day 11 part 1" day11_1 "inputs/day11.input" 54253
   ,labeledDayTestCase "day 11 part 2" day11_2 "inputs/day11.input" 13119526120
   ,labeledDayTestCase "day 12 part 1" day12_1 "inputs/day12.input" 472
   ,labeledDayTestCase "day 12 part 2" day12_2 "inputs/day12.input" 465
   ,labeledDayTestCase "day 13 part 1" day13_1 "inputs/day13.input" 6046
   ,labeledDayTestCase "day 13 part 2" day13_2 "inputs/day13.input" 21423
   ,labeledDayTestCase "day 14 part 1" day14_1 "inputs/day14.input" 888
   ,labeledDayTestCase "day 14 part 2" day14_2 "inputs/day14.input" (-1)
  ]
