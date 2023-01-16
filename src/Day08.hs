module Day08 (day08_1, day08_2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Flow

type Tree = (Int, Int)
type Grid = [[Int]]

minTreeHeight :: Int
minTreeHeight = 0

maxTreeHeight :: Int
maxTreeHeight = 9

day08_1 :: [String] -> Int
day08_1 input = do
  let grid :: Grid = map (map (\c -> read [c])) input
  let w = width grid
  let h = height grid
  let topDown = map (, 0::Int) [0..w-1] |> map (\t -> map (fst t, ) [0..h-1]) |> map (countVisible grid) |> Set.unions
  let rightToLeft = map (w-1, ) [0..h-1] |> map (\t -> map (, snd t) [(w-1), (w-2)..0]) |> map (countVisible grid) |> Set.unions
  let bottomUp = map (, h-1) [0..w-1] |> map (\t -> map (fst t, ) [h-1,h-2..0]) |> map (countVisible grid) |> Set.unions
  let leftToRight = map (0::Int, ) [0..h-1] |> map (\t -> map (, snd t) [0..w-1]) |> map (countVisible grid) |> Set.unions
  Set.size (Set.unions [topDown, rightToLeft, bottomUp, leftToRight])

countVisible :: Grid -> [Tree] -> Set Tree  -- grid -> positions -> visible trees
countVisible = doCountVisible Set.empty (minTreeHeight - 1)

doCountVisible :: Set Tree -> Int -> Grid -> [Tree] -> Set Tree  -- visible trees so far -> currMaxHeight -> grid -> positions -> visible trees
doCountVisible visible _ _ [] = visible
doCountVisible visible currMaxHeight grid (tree:rest)
  | currMaxHeight == maxTreeHeight = visible  -- we'll not find any higher tree and can stop here
  | otherwise = do
    if treeHeight grid tree > currMaxHeight
      then doCountVisible (Set.insert tree visible) (treeHeight grid tree) grid rest
      else doCountVisible visible currMaxHeight grid rest

day08_2 :: [String] -> Int
day08_2 input = do
  let grid :: Grid = map (map (\c -> read [c])) input
  let interestingTrees = [(x,y) | x <- [1..width grid-2], y <- [1..height grid-2]]  -- edges, by definition have scenicScore = 0, because there are no trees in at least one direction
  map (calcScenicScore grid) interestingTrees |> maximum

calcScenicScore :: Grid -> Tree -> Int
calcScenicScore grid tree = do
  let visibleToTheTop = countVisibleInDirection grid (gridUp grid) tree
  let visibleToTheBottom = countVisibleInDirection grid (gridDown grid) tree
  let visibleToTheLeft = countVisibleInDirection grid (gridLeft grid) tree
  let visibleToTheRight = countVisibleInDirection grid (gridRight grid) tree
  visibleToTheTop * visibleToTheBottom * visibleToTheLeft * visibleToTheRight

type Direction = Tree -> Maybe Tree -- tree -> maybe another tree

countVisibleInDirection :: Grid -> Direction -> Tree -> Int -- grid -> dir -> tree -> count
countVisibleInDirection grid dir tree = do
  doCountVisibleInDirection grid dir (treeHeight grid tree) (dir tree) 0

doCountVisibleInDirection :: Grid -> Direction -> Int -> Maybe Tree -> Int -> Int  -- grid -> dir -> houseTreeHeight -> tree -> countedSoFar ->  count
doCountVisibleInDirection _ _ _ Nothing countedSoFar = countedSoFar
doCountVisibleInDirection grid dir houseTreeHeight (Just tree) countedSoFar
  | houseTreeHeight <= treeHeight grid tree = countedSoFar + 1
  | otherwise = doCountVisibleInDirection grid dir houseTreeHeight (dir tree) (countedSoFar + 1)

gridUp :: Grid -> Direction
gridUp _ pos
  | snd pos > 0 = Just (fst pos, snd pos - 1)
  | otherwise = Nothing

gridDown :: Grid -> Direction
gridDown grid pos
  | snd pos < height grid - 1 = Just (fst pos, snd pos + 1)
  | otherwise = Nothing

gridLeft :: Grid -> Direction
gridLeft _ pos
  | fst pos > 0 = Just (fst pos - 1, snd pos)
  | otherwise = Nothing

gridRight :: Grid -> Direction
gridRight grid pos
  | fst pos < width grid - 1 = Just (fst pos + 1, snd pos)
  | otherwise = Nothing

width :: Grid -> Int
width grid = length (head grid)

height :: Grid -> Int
height = length

treeHeight :: Grid -> Tree -> Int
treeHeight grid tree = grid !! snd tree !! fst tree
