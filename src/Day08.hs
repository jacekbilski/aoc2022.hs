module Day08 (day08_1, day08_2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Flow

type XY = (Int, Int)

minTreeHeight :: Int
minTreeHeight = 0

maxTreeHeight :: Int
maxTreeHeight = 9

-- idea: treat input as [[Int]]
-- walk from all 4 directions through all columns/rows, if a tree is visible, add its coordinates to a set of visible trees
-- result is the size of the set

day08_1 :: [String] -> Int
day08_1 input = do
  let grid :: [[Int]] = map (map (\c -> read [c])) input
  let width = length (head grid)
  let height = length grid
  let topDown = map (, 0::Int) [0..width-1] |> map (\t -> map (fst t, ) [0..height-1]) |> map (countVisible grid) |> Set.unions
  let rightToLeft = map (width-1, ) [0..height-1] |> map (\t -> map (, snd t) [(width-1), (width-2)..0]) |> map (countVisible grid) |> Set.unions
  let bottomUp = map (, height-1) [0..width-1] |> map (\t -> map (fst t, ) [height-1,height-2..0]) |> map (countVisible grid) |> Set.unions
  let leftToRight = map (0::Int, ) [0..height-1] |> map (\t -> map (, snd t) [0..width-1]) |> map (countVisible grid) |> Set.unions
  Set.size (Set.unions [topDown, rightToLeft, bottomUp, leftToRight])

countVisible :: [[Int]] -> [XY] -> Set XY  -- grid -> positions -> visible trees
countVisible = doCountVisible Set.empty (minTreeHeight - 1)

doCountVisible :: Set XY -> Int -> [[Int]] -> [XY] -> Set XY  -- visible trees so far -> currMaxHeight -> grid -> positions -> visible trees
doCountVisible found _ _ [] = found
doCountVisible found currMaxHeight grid (pos:rest)
  | currMaxHeight == maxTreeHeight = found  -- we'll not find any higher tree and can stop here
  | otherwise = do
    let tree = grid !! snd pos !! fst pos
    if tree > currMaxHeight
      then doCountVisible (Set.insert pos found) tree grid rest
      else doCountVisible found currMaxHeight grid rest

day08_2 :: [String] -> IO Int
day08_2 input = do
  let grid :: [[Int]] = map (map (\c -> read [c])) input
  let width = length (head grid)
  let height = length grid
  let interestingTrees = [(x,y) | x <- [1..width-2], y <- [1..height-2]]
  return (map (calcScenicScore grid) interestingTrees |> maximum)

calcScenicScore :: [[Int]] -> XY -> Int
calcScenicScore grid tree = do
  -1

type Direction = Int -> Int -> XY -> Maybe XY -- gridWidth -> gridHeight -> pos -> newPos

up :: Direction
up _ _ pos
  | snd pos > 0 = Just (fst pos, snd pos - 1)
  | otherwise = Nothing

down :: Direction
down _ gridHeight pos
  | snd pos < gridHeight = Just (fst pos, snd pos + 1)
  | otherwise = Nothing
