module Day04 (day04_1, day04_2) where

import Data.List.Split
import Flow

type Range = (Int, Int)

day04_1 :: [String] -> Int
day04_1 input = map parseInput input |> filter rangesOverlapping |> length

parseInput :: String -> (Range, Range)
parseInput input = do
  let arr = splitOn "," input |> map parseRange
  (arr !! 0, arr !! 1)

parseRange :: String -> Range
parseRange input = do
  let arr = splitOn "-" input
  (read (arr !! 0), read (arr !! 1))

rangesOverlapping :: (Range, Range) -> Bool
rangesOverlapping (x, y) = (fst x <= fst y && snd x >= snd y) || (fst x >= fst y && snd x <= snd y)

day04_2 :: [String] -> Int
day04_2 input = undefined
