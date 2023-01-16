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
moveTail t h = doMoveTail t (diff h t)

diff :: Coords -> Coords -> Coords
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

doMoveTail :: Coords -> Coords -> Coords
doMoveTail t (dx, dy)
  | abs dx + abs dy > 1 = reallyDoMoveTail t (dx, dy)
  | otherwise = t

reallyDoMoveTail :: Coords -> Coords -> Coords
reallyDoMoveTail (tx, ty) (2, 0) = (tx + 1, ty)
reallyDoMoveTail (tx, ty) (-2, 0) = (tx - 1, ty)
reallyDoMoveTail (tx, ty) (0, 2) = (tx, ty + 1)
reallyDoMoveTail (tx, ty) (0, -2) = (tx, ty - 1)
reallyDoMoveTail (tx, ty) (1, 1) = (tx, ty)
reallyDoMoveTail (tx, ty) (1, -1) = (tx, ty)
reallyDoMoveTail (tx, ty) (-1, 1) = (tx, ty)
reallyDoMoveTail (tx, ty) (-1, -1) = (tx, ty)

day09_2 :: [String] -> Int
day09_2 _ = undefined
