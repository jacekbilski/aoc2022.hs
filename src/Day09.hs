module Day09 (day09_1, day09_2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

type Coords = (Int, Int)

data Direction = Up | Down | Left | Right

type Instruction = (Direction, Int)

day09_1 :: [String] -> IO Int
day09_1 input = do
  let instructions = parseInput input
  let visited = walk (Set.fromList [(0, 0)]) (0, 0) (0, 0) instructions
  return (Set.size visited)

parseInput :: [String] -> [Instruction]
parseInput = map (\i -> (parseDirection (head i), read (drop 2 i)))

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Left
parseDirection 'R' = Right

walk :: Set Coords -> Coords -> Coords -> [Instruction] -> Set Coords
walk visited _ _ [] = visited
walk visited h t ((_, 0) : is) = walk visited h t is
walk visited h t ((dir, steps) : is) = do
  let newHead = moveHead h dir
  let newTail = moveTail t newHead
  walk (Set.insert newTail visited) newHead newTail ((dir, steps - 1) : is)

moveHead :: Coords -> Direction -> Coords
moveHead (x, y) Up = (x, y + 1)
moveHead (x, y) Down = (x, y - 1)
moveHead (x, y) Left = (x - 1, y)
moveHead (x, y) Right = (x + 1, y)

moveTail :: Coords -> Coords -> Coords
moveTail t h = t

day09_2 :: [String] -> Int
day09_2 _ = undefined
