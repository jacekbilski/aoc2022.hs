module Day09 (day09_1, day09_2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Flow
import Prelude hiding (Left, Right)

type Coords = (Int, Int)

data Direction = Up | Down | Left | Right

type Instruction = (Direction, Int)

day09_1 :: [String] -> Int
day09_1 input = do
  let instructions = parseInput input
  walk (Set.fromList [(0, 0)]) [(0, 0), (0, 0)] instructions |> Set.size

parseInput :: [String] -> [Instruction]
parseInput = map (\i -> (parseDirection (head i), read (drop 2 i)))

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Left
parseDirection 'R' = Right
parseDirection dir = error ("Unknown direction " ++ [dir])

walk :: Set Coords -> [Coords] -> [Instruction] -> Set Coords
walk visited _ [] = visited
walk visited rope ((_, 0) : is) = walk visited rope is
walk visited rope ((dir, steps) : is) = do
  let newRope = moveRope rope dir
  walk (Set.insert (last newRope) visited) newRope ((dir, steps - 1) : is)

moveRope :: [Coords] -> Direction -> [Coords]
moveRope rope dir = doMoveRope dir [] rope

doMoveRope :: Direction -> [Coords] -> [Coords] -> [Coords]
doMoveRope _ ropeHead [] = reverse ropeHead
doMoveRope dir [] (this : ropeTail) = doMoveRope dir [moveHead this dir] ropeTail
doMoveRope dir (previous : ropeHead) (this : ropeTail) = doMoveRope dir (moveKnot this previous : previous : ropeHead) ropeTail

moveHead :: Coords -> Direction -> Coords
moveHead (x, y) Up = (x, y + 1)
moveHead (x, y) Down = (x, y - 1)
moveHead (x, y) Left = (x - 1, y)
moveHead (x, y) Right = (x + 1, y)

moveKnot :: Coords -> Coords -> Coords
moveKnot this previous = doMoveKnot this (diff previous this)

diff :: Coords -> Coords -> Coords
diff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

doMoveKnot :: Coords -> Coords -> Coords
doMoveKnot t (dx, dy)
  | abs dx + abs dy > 1 = reallyDoMoveKnot t (dx, dy)
  | otherwise = t

reallyDoMoveKnot :: Coords -> Coords -> Coords
reallyDoMoveKnot (x, y) (1, 1) = (x, y)
reallyDoMoveKnot (x, y) (1, -1) = (x, y)
reallyDoMoveKnot (x, y) (-1, 1) = (x, y)
reallyDoMoveKnot (x, y) (-1, -1) = (x, y)
reallyDoMoveKnot (x, y) (dx, dy) = (x + signum dx, y + signum dy)

day09_2 :: [String] -> Int
day09_2 input = do
  let rope = repeat (0, 0) |> take 10
  walk (Set.fromList [(0, 0)]) rope (parseInput input) |> Set.size
