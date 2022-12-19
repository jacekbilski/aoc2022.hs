module AoC2022 (day01) where

import System.IO
import Control.Exception

day01 :: IO ()
day01 = bracket (openFile "inputs/day01.input1" ReadMode) hClose
                (\h -> do contents <- hGetContents h
                          let maxCalories = findMaxCalories contents
                          putStrLn (show maxCalories))

-- Basic idea: single go through the input
-- in accumulator I keep: current max calories of some elf I processed already and sum of calories for current elf
-- folding: in case element is a number -> increase sum of calories for the current given elf
--  in case element is empty or no more elements -> take highest from the values in the tuple

newValue :: String -> (Integer, Integer) -> (Integer, Integer)
newValue "" (currMax, curr) = (maximum [currMax, curr], 0)
newValue x (currMax, curr) = (currMax, curr + (read x))

findMaxCalories :: String -> Integer
findMaxCalories input = do
  let acc = foldr newValue (0, 0) (lines input)
  maximum [(fst acc), (snd acc)]
