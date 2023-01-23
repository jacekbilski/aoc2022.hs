module Day12 (day12_1, day12_2) where

import Data.Bifunctor
import Data.Char (ord)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Flow
import Prelude hiding (round)

type X = Int

type Y = Int

type Coords = (X, Y)

type Heightmap = [[Int]]

type Visited = Map Coords Int

day12_1 :: [String] -> String
day12_1 input = do
  let (heightmap, start, end) = parseInput input
  show (walk heightmap start end)

parseInput :: [[Char]] -> (Heightmap, Coords, Coords)
parseInput input = do
  let start = map (elemIndex 'S') input |> zip [0 ..] |> filter (isJust . snd) |> map (Data.Bifunctor.second fromJust) |> map (\t -> (snd t, fst t)) |> head
  let end = map (elemIndex 'E') input |> zip [0 ..] |> filter (isJust . snd) |> map (Data.Bifunctor.second fromJust) |> map (\t -> (snd t, fst t)) |> head
  let heightmap = map (map mapHeight) input
  (heightmap, start, end)

mapHeight :: Char -> Int
mapHeight 'S' = mapHeight 'a'
mapHeight 'E' = mapHeight 'z'
mapHeight c = ord c

walk :: Heightmap -> Coords -> Coords -> Int
walk heightmap start end = do
  let notVisited = filter (/= start) [(x, y) | x <- [0 .. (head heightmap |> length) - 1], y <- [0 .. (length heightmap - 1)]]
  let visited = doWalk heightmap end 1 (Map.fromList [(start, 0)]) notVisited
  visited Map.! end

doWalk :: Heightmap -> Coords -> Int -> Visited -> [Coords] -> Visited
doWalk heightmap end round visited notVisited
  | Map.member end visited = visited
  | otherwise = do
    let (newVisited, newNotVisited) = step heightmap end round visited notVisited
    doWalk heightmap end (round + 1) newVisited newNotVisited

step :: Heightmap -> Coords -> Int -> Visited -> [Coords] -> (Visited, [Coords])
step heightmap end round visited = doStep heightmap end round visited []

doStep :: Heightmap -> Coords -> Int -> Visited -> [Coords] -> [Coords] -> (Visited, [Coords])
doStep _ _ _ visited checked [] = (visited, checked)
doStep heightmap end round visited checked (current : toCheck)
  | isReachableFrom heightmap visited end = doStep heightmap end round (Map.insert current round visited) checked toCheck
  | otherwise = doStep heightmap end round visited (current : checked) toCheck

isReachableFrom :: Heightmap -> Visited -> Coords -> Bool
isReachableFrom heightmap visited current = do
  let heightAt = at heightmap
  let possibilities = [(fst current, snd current -1), (fst current, snd current + 1), (fst current -1, snd current), (fst current + 1, snd current)]
  filter (`Map.member` visited) possibilities |> map heightAt |> any (>= heightAt current - 1)

at :: Heightmap -> Coords -> Int
at heightmap coords = heightmap !! snd coords !! fst coords

day12_2 :: [String] -> String
day12_2 _ = "Nothing yet"
