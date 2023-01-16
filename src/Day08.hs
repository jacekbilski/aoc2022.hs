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

day08_2 :: [String] -> Int
day08_2 input = do
  let grid :: [[Int]] = map (map (\c -> read [c])) input
  let width = length (head grid)
  let height = length grid
  let interestingTrees = [(x,y) | x <- [1..width-2], y <- [1..height-2]]  -- edges, by definition have scenicScore = 0, because there are no trees in at least one direction
  map (calcScenicScore grid) interestingTrees |> maximum

calcScenicScore :: [[Int]] -> XY -> Int
calcScenicScore grid tree = do
  let width = length (head grid)
  let height = length grid
  let visibleToTheTop = countVisibleInDirection grid (up width height) tree
  let visibleToTheBottom = countVisibleInDirection grid (down width height) tree
  let visibleToTheLeft = countVisibleInDirection grid (left width height) tree
  let visibleToTheRight = countVisibleInDirection grid (right width height) tree
  visibleToTheTop * visibleToTheBottom * visibleToTheLeft * visibleToTheRight

type Direction = Int -> Int -> XY -> Maybe XY -- gridWidth -> gridHeight -> pos -> newPos

countVisibleInDirection :: [[Int]] -> (XY -> Maybe XY) -> XY -> Int -- grid -> dir -> tree/pos -> count
countVisibleInDirection grid dir pos = do
  let treeHeight = grid !! snd pos !! fst pos
  doCountVisibleInDirection grid dir treeHeight (dir pos) 0

doCountVisibleInDirection :: [[Int]] -> (XY -> Maybe XY) -> Int -> Maybe XY -> Int -> Int  -- grid -> dir -> treeHeight -> pos -> countedSoFar ->  count
doCountVisibleInDirection _ _ _ Nothing countedSoFar = countedSoFar
doCountVisibleInDirection grid dir treeHeight (Just pos) countedSoFar
  | treeHeight <= (grid !! snd pos !! fst pos) = countedSoFar + 1
  | otherwise = doCountVisibleInDirection grid dir treeHeight (dir pos) (countedSoFar + 1)

up :: Direction
up _ _ pos
  | snd pos > 0 = Just (fst pos, snd pos - 1)
  | otherwise = Nothing

down :: Direction
down _ gridHeight pos
  | snd pos < gridHeight - 1 = Just (fst pos, snd pos + 1)
  | otherwise = Nothing

left :: Direction
left _ _ pos
  | fst pos > 0 = Just (fst pos - 1, snd pos)
  | otherwise = Nothing

right :: Direction
right gridWidth _ pos
  | fst pos < gridWidth - 1 = Just (fst pos + 1, snd pos)
  | otherwise = Nothing
