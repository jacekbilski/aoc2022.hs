module Day13 (day13_1, day13_2) where

import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Flow

data D = N Int | L [D]

type Packet = D

type Pair = (Packet, Packet)

instance Show D where
  show (N x) = show x
  show (L x) = show x

day13_1 :: [String] -> Int
day13_1 input = do
  let pairs = parseInput input
  map (uncurry isRightOrder) pairs |> map fromJust |> zip [1..] |> filter snd |> map fst |> sum

parseInput :: [String] -> [Pair]
parseInput input = chunksOf 3 input |> map parsePair

parsePair :: [String] -> Pair
parsePair input = (parsePart (head input), parsePart (input !! 1))

parsePart :: String -> D
parsePart part
  | null part = L []
  | head part == '[' = take (length part - 2) (tail part) |> split ',' |> map parsePart |> L
  | otherwise = N (read part)

split :: Char -> String -> [String]
split delim = doSplit delim [] ""

doSplit :: Char -> [String] -> [Char] -> [Char] -> [String]
doSplit _ done current [] = (current : done) |> reverse
doSplit delim done current (c : rest)
  | c == delim = doSplit delim (current : done) "" rest
  | c == '[' = do
    let matchingBracketIdx = findMatchingBracket rest
    doSplit delim done (current ++ [c] ++ take matchingBracketIdx rest) (drop matchingBracketIdx rest)
  | otherwise = doSplit delim done (current ++ [c]) rest

findMatchingBracket :: [Char] -> Int
findMatchingBracket input = doFindMatchingBracket input 1 0

doFindMatchingBracket :: [Char] -> Int -> Int -> Int
doFindMatchingBracket _ 0 idx = idx
doFindMatchingBracket (c : cx) bracketsToFind idx
  | c == '[' = doFindMatchingBracket cx (bracketsToFind + 1) (idx + 1)
  | c == ']' = doFindMatchingBracket cx (bracketsToFind - 1) (idx + 1)
  | otherwise = doFindMatchingBracket cx bracketsToFind (idx + 1)

isRightOrder :: D -> D -> Maybe Bool
isRightOrder (N l) (N r)
  | l < r = Just True
  | l > r = Just False
  | otherwise = Nothing
isRightOrder (L (l:lx)) (L (r:rx)) = do
  let inOrder = isRightOrder l r
  case inOrder of
    Just b -> Just b
    Nothing -> isRightOrder (L lx) (L rx)
isRightOrder (L []) (L []) = Nothing
isRightOrder (L []) (L _) = Just True
isRightOrder (L _) (L []) = Just False
isRightOrder (L l) (N r) = isRightOrder (L l) (L [N r])
isRightOrder (N l) (L r) = isRightOrder (L [N l]) (L r)

day13_2 :: [String] -> Int
day13_2 _ = undefined
