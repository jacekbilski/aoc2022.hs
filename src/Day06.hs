module Day06 (day06_1, day06_2) where

import Flow
import qualified Data.Set as Set

day06_1 :: [String] -> Int
day06_1 input = head input |> findStartOfX 4 4

findStartOfX :: Int -> Int -> String -> Int
findStartOfX markerSize toSkip dataStream = if allDifferent (take markerSize dataStream) then toSkip else findStartOfX markerSize (toSkip + 1) (tail dataStream)

allDifferent :: [Char] -> Bool
allDifferent chars = (Set.fromList chars |> Set.size) == length chars

day06_2 :: [String] -> Int
day06_2 input = head input |> findStartOfX 14 14
