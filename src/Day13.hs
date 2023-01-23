module Day13 (day13_1, day13_2) where

import Data.List.Split (chunksOf, splitOn)
import Flow

data D = N Int | L [D]
type Packet = D
type Pair = (Packet, Packet)

instance Show D where
  show (N x) = show x
  show (L x) = show x

day13_1 :: [String] -> String
day13_1 input = do
  let pairs = parseInput input
  show pairs

parseInput :: [String] -> [Pair]
parseInput input = chunksOf 3 input |> map parsePair

parsePair :: [String] -> Pair
parsePair input = (parsePart (head input), parsePart (input !! 1))

parsePart :: String -> D
parsePart part
  | head part == '[' = take (length part - 2) (tail part) |> split ',' |> map parsePart |> L
  | otherwise = N (read part)

split :: Char -> String -> [String]
split char = splitOn [char]

day13_2 :: [String] -> Int
day13_2 _ = undefined
