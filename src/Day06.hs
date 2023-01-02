module Day06 (day06_1, day06_2) where

import Flow

day06_1 :: [String] -> Int
day06_1 input = do
  let datastream = head input
  find_start_of_packet datastream

find_start_of_packet :: String -> Int
find_start_of_packet datastream = do_find_start_of_packet 4 datastream

do_find_start_of_packet :: Int -> String -> Int
do_find_start_of_packet to_skip datastream = if (all_different (take 4 datastream)) then to_skip else do_find_start_of_packet (to_skip + 1) (tail datastream)

all_different :: [Char] -> Bool
all_different [] = True
all_different (x:[]) = True
all_different (x:xs) = ((filter (\e -> e == x) xs |> length) == 0) && all_different xs

day06_2 :: [String] -> Int
day06_2 input = undefined
