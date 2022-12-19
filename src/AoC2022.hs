module AoC2022 (solve, day01_1) where

import System.IO
import Control.Exception

solve :: (String -> Integer) -> String -> IO () -- function per day, input file name -> result
solve day fileName = bracket (openFile fileName ReadMode) hClose
                (\h -> do contents <- hGetContents h
                          let result = day contents
                          putStrLn (show result))

-- Basic idea: single go through the input
-- in accumulator I keep: current max calories of some elf I processed already and sum of calories for current elf
-- folding: in case element is a number -> increase sum of calories for the current given elf
--  in case element is empty or no more elements -> take highest from the values in the tuple

day01_1 :: String -> Integer
day01_1 input = maximum (sumCalories input)

-- For part 2 I need to go back to the original idea: simply sum the calories carried by each elf in a list.
-- Them simply sort, revert, take 3, sum

addCalories :: String -> [Integer] -> [Integer]
addCalories "" xs = 0 : xs
addCalories val [] = [(read val)]
addCalories val (x:[]) = [x + (read val)]
addCalories val (x:xs) = x + (read val) : xs

sumCalories :: String -> [Integer]
sumCalories input = foldr addCalories [] (lines input)
