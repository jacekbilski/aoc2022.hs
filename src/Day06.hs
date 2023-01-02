module Day06 (day06_1, day06_2) where

import Flow
import qualified Data.Set as Set

day06_1 :: [String] -> Int
day06_1 input = head input |> find_start_of_x 4 4

find_start_of_x :: Int -> Int -> String -> Int
find_start_of_x marker_size to_skip datastream = if (all_different (take marker_size datastream)) then to_skip else find_start_of_x marker_size (to_skip + 1) (tail datastream)

all_different :: [Char] -> Bool
all_different chars = (Set.fromList chars |> Set.size) == length chars

day06_2 :: [String] -> Int
day06_2 input = head input |> find_start_of_x 14 14
