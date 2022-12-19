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
day01_1 input = findMaxCalories input

-- For part 2 I need to go back to the original idea: simply sum the calories carried by each elf in a list.
-- Them simply sort, revert, take 3, sum

newValue :: String -> (Integer, Integer) -> (Integer, Integer)
newValue "" (currMax, curr) = (maximum [currMax, curr], 0)
newValue x (currMax, curr) = (currMax, curr + (read x))

findMaxCalories :: String -> Integer
findMaxCalories input = do
  let acc = foldr newValue (0, 0) (lines input)
  maximum [(fst acc), (snd acc)]
