module Day01 (day01_1, day01_2) where

import Data.List (sort)
import Flow

-- Basic idea: single go through the input
-- in accumulator I keep: current max calories of some elf I processed already and sum of calories for current elf
-- folding: in case element is a number -> increase sum of calories for the current given elf
--  in case element is empty or no more elements -> take highest from the values in the tuple

day01_1 :: [String] -> Int
day01_1 input = sumCalories input |> maximum

-- For part 2 I need to go back to the original idea: simply sum the calories carried by each elf in a list.
-- Them simply sort, revert, take 3, sum

day01_2 :: [String] -> Int
day01_2 input = sumCalories input |> sort |> reverse |> take 3 |> sum

addCalories :: String -> [Int] -> [Int]
addCalories "" xs = 0 : xs
addCalories val [] = [read val]
addCalories val [x] = [x + read val]
addCalories val (x:xs) = x + read val : xs

sumCalories :: [String] -> [Int]
sumCalories = foldr addCalories []
