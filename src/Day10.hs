module Day10 (day10_1, day10_2) where

import Data.List (isPrefixOf)
import Data.List.Split (chunksOf)
import Flow
import Prelude hiding (cycle)

type RegisterValue = (Int, Int) -- actual value, valid for cycles

screenWidth :: Int
screenWidth = 40

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

day10_2 :: [String] -> [String]
day10_2 input = do
  let registerHistory = parseInstructions input
  draw registerHistory

draw :: [RegisterValue] -> [String]
draw registerHistory = doDraw "" 0 registerHistory |> reverse |> chunksOf screenWidth

doDraw :: String -> Int -> [RegisterValue] -> String
doDraw display _ [] = display
doDraw display currentCycle ((_, 0):rvs) = doDraw display currentCycle rvs
doDraw display currentCycle (rv:rvs)
  | abs (spriteMiddle - horizontalPosition) <= 1 = doDraw ('#':display) (currentCycle + 1) ((fst rv, snd rv - 1):rvs)
  | otherwise = doDraw ('.':display) (currentCycle + 1) ((fst rv, snd rv - 1):rvs)
  where spriteMiddle = fst rv
        horizontalPosition = currentCycle `rem` screenWidth
