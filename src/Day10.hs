module Day10 (day10_1, day10_2) where

import Data.List (isPrefixOf)
import Flow
import Prelude hiding (cycle)

type RegisterValue = (Int, Int) -- actual value, valid for cycles

day10_1 :: [String] -> Int
day10_1 input = do
  let registerHistory = parseInstructions input
  map (`signalStrengthAtCycle` registerHistory) [20, 60, 100, 140, 180, 220] |> sum

parseInstructions :: [String] -> [RegisterValue]
parseInstructions input = doParseInstructions [(1, 0)] input |> reverse

doParseInstructions :: [RegisterValue] -> [String] -> [RegisterValue]
doParseInstructions rvs [] = rvs
doParseInstructions (rv : rvs) (i : is)
  | i == "noop" = doParseInstructions ((fst rv, snd rv + 1) : rvs) is
  | "addx" `isPrefixOf` i = doParseInstructions ((read (drop 5 i) + fst rv, 0) : (fst rv, snd rv + 2) : rvs) is

signalStrengthAtCycle :: Int -> [RegisterValue] -> Int
signalStrengthAtCycle cycle = doSignalStrengthAtCycle cycle 0

doSignalStrengthAtCycle :: Int -> Int -> [RegisterValue] -> Int
doSignalStrengthAtCycle cycle currentCycle (rv:rvs)
  | cycle <= currentCycle + snd rv = cycle * fst rv
  | otherwise = doSignalStrengthAtCycle cycle (currentCycle + snd rv) rvs

day10_2 :: [String] -> Int
day10_2 _ = undefined
