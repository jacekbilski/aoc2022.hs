module Day08 (day08_1, day08_2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Flow

type Tree = (Int, Int)
type Grid = [[Int]]

type Direction = Tree -> Maybe Tree

minTreeHeight :: Int
minTreeHeight = 0

maxTreeHeight :: Int
maxTreeHeight = 9

day08_1 :: [String] -> Int
day08_1 input = do
  let grid = getGrid input
  let w = width grid
  let h = height grid
  let topDown = map (, 0::Int) [0..w-1] |> map (countVisible (gridDown grid) grid) |> Set.unions
  let rightToLeft = map (w-1, ) [0..h-1] |> map (countVisible (gridLeft grid) grid) |> Set.unions
  let bottomUp = map (, h-1) [0..w-1] |> map (countVisible (gridUp grid) grid) |> Set.unions
  let leftToRight = map (0::Int, ) [0..h-1] |> map (countVisible (gridRight grid) grid) |> Set.unions
  Set.size (Set.unions [topDown, rightToLeft, bottomUp, leftToRight])

countVisible :: Direction -> Grid -> Tree -> Set Tree  -- direction -> grid -> starting tree -> visible trees
countVisible dir grid startingTree = doCountVisible dir grid (Just startingTree) Set.empty (minTreeHeight - 1)

doCountVisible :: Direction -> Grid -> Maybe Tree -> Set Tree -> Int -> Set Tree  -- direction -> grid -> currentTree -> visible trees so far -> currMaxHeight -> visible trees
doCountVisible _ _ Nothing visible _ = visible
doCountVisible dir grid (Just tree) visible currMaxHeight
  | currMaxHeight == maxTreeHeight = visible  -- we'll not find any higher tree and can stop here
  | currMaxHeight < treeHeight grid tree = doCountVisible dir grid (dir tree) (Set.insert tree visible) (treeHeight grid tree)
  | otherwise = doCountVisible dir grid (dir tree) visible currMaxHeight

day08_2 :: [String] -> Int
day08_2 input = do
  let grid = getGrid input
  let interestingTrees = [(x,y) | x <- [1..width grid-2], y <- [1..height grid-2]]  -- edges, by definition have scenicScore = 0, because there are no trees in at least one direction
  map (calcScenicScore grid) interestingTrees |> maximum

calcScenicScore :: Grid -> Tree -> Int
calcScenicScore grid tree = do
  let visibleToTheTop = countVisibleInDirection (gridUp grid) grid tree
  let visibleToTheBottom = countVisibleInDirection (gridDown grid) grid tree
  let visibleToTheLeft = countVisibleInDirection (gridLeft grid) grid tree
  let visibleToTheRight = countVisibleInDirection (gridRight grid) grid tree
  visibleToTheTop * visibleToTheBottom * visibleToTheLeft * visibleToTheRight

countVisibleInDirection :: Direction -> Grid -> Tree -> Int -- grid -> dir -> tree -> count
countVisibleInDirection dir grid tree = doCountVisibleInDirection dir grid (treeHeight grid tree) (dir tree) 0

doCountVisibleInDirection :: Direction -> Grid -> Int -> Maybe Tree -> Int -> Int  -- grid -> dir -> houseTreeHeight -> tree -> countedSoFar -> count
doCountVisibleInDirection _ _ _ Nothing countedSoFar = countedSoFar
doCountVisibleInDirection dir grid houseTreeHeight (Just tree) countedSoFar
  | houseTreeHeight <= treeHeight grid tree = countedSoFar + 1
  | otherwise = doCountVisibleInDirection dir grid houseTreeHeight (dir tree) (countedSoFar + 1)

gridUp :: Grid -> Direction
gridUp _ (x, y)
  | y > 0 = Just (x, y - 1)
  | otherwise = Nothing

gridDown :: Grid -> Direction
gridDown grid (x, y)
  | y < height grid - 1 = Just (x, y + 1)
  | otherwise = Nothing

gridLeft :: Grid -> Direction
gridLeft _ (x, y)
  | x > 0 = Just (x - 1, y)
  | otherwise = Nothing

gridRight :: Grid -> Direction
gridRight grid (x, y)
  | x < width grid - 1 = Just (x + 1, y)
  | otherwise = Nothing

getGrid :: [String] -> Grid
getGrid = map (map (\c -> read [c]))

width :: Grid -> Int
width grid = length (head grid)

height :: Grid -> Int
height = length

treeHeight :: Grid -> Tree -> Int
treeHeight grid (x, y) = grid !! y !! x
