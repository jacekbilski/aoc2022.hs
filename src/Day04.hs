module Day04 (day04_1, day04_2) where

import Data.List.Split
import Flow

type Range = (Int, Int)

day04_1 :: [String] -> Int
day04_1 input = map parseInput input |> filter rangesFullyOverlap |> length

parseInput :: String -> (Range, Range)
parseInput input = do
  let arr = splitOn "," input |> map parseRange
  (arr !! 0, arr !! 1)

parseRange :: String -> Range
parseRange input = do
  let arr = splitOn "-" input
  (read (arr !! 0), read (arr !! 1))

rangesFullyOverlap :: (Range, Range) -> Bool
rangesFullyOverlap (x, y) = (fst x <= fst y && snd x >= snd y) || (fst x >= fst y && snd x <= snd y)

day04_2 :: [String] -> Int
day04_2 input = map parseInput input |> filter rangesPartiallyOverlap |> length

rangesPartiallyOverlap :: (Range, Range) -> Bool
rangesPartiallyOverlap (x, y) = do
  let diff1 = snd y - fst x
  let diff2 = fst y - snd x
  diff1 == 0 || diff2 == 0 || signum diff1 /= signum diff2
