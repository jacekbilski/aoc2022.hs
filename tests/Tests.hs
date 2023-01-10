module Tests where

import AoC2022
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map

dayTestCase :: (Show a, Eq a) => ([String] -> a) -> FilePath -> a -> Assertion
dayTestCase day input expectedResult = do
  result <- solve input day
  assertEqual ("It's " ++ show expectedResult) expectedResult result

labeledDayTestCase :: (Show a, Eq a) => String -> ([String] -> a) -> FilePath -> a -> TestTree
labeledDayTestCase name day input expectedResult = testCase name $ dayTestCase day input expectedResult

dayExampleTestCase :: (Show a, Eq a) => ([String] -> a) -> [String] -> a -> Assertion
dayExampleTestCase day input expectedResult = do
  let result = day input
  assertEqual ("It's " ++ show expectedResult) expectedResult result

labeledDayExampleTestCase :: (Show a, Eq a) => String -> ([String] -> a) -> [String] -> a -> TestTree
labeledDayExampleTestCase name day input expectedResult = testCase name $ dayExampleTestCase day input expectedResult

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
   ,labeledDayTestCase "day 07 part 2" day07_2 "inputs/day07.input" (-1)
   ,labeledDayExampleTestCase "day 07 example 1" day07_1 ["$ cd /"] 0
   ,labeledDayExampleTestCase "day 07 example 2" day07_1 ["$ cd /", "$ ls", "123 x"] 123
   ,labeledDayExampleTestCase "day 07 example 3" day07_1 ["$ cd /", "$ ls", "123 x", "123 y"] 246
   ,labeledDayExampleTestCase "day 07 example 4" day07_1 ["$ cd /", "$ ls", "dir a", "$ cd a", "123 x"] 246
  ]

unit_getFile1 :: Assertion
unit_getFile1 = do
  let root = Directory Map.empty
  let foundRoot = getFile [] root
--  print ("getFile1 'getFile [] root': " ++ show foundRoot)
  case foundRoot of
    Directory dir -> assertBool "abc" (Map.null dir)

unit_getFile2 :: Assertion
unit_getFile2 = do
  let root = Directory Map.empty
--  print ("getFile2 root: " ++ show root)
  let file = RegularFile 1
  let newRoot = addFile "a" file [] root
--  print ("getFile2 'addFile \"a\" file [] root': " ++ show newRoot)
  let foundRoot = getFile [] newRoot
--  print ("getFile2 'getFile [] newRoot': " ++ show foundRoot)
  case foundRoot of
    Directory dir -> do
      assertEqual "size" 1 (Map.size dir)

unit_findDirs :: Assertion
unit_findDirs = do
  let root = Directory Map.empty
  let dirs = findDirs root
--  print dirs
  assertEqual "size" 1 (length dirs)
