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

day12_1 :: [String] -> Int
day12_1 input = do
  let (heightmap, start, end) = parseInput input
  let visited = walk heightmap start
  visited Map.! end

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

walk :: Heightmap -> Coords -> Visited
walk heightmap start = do
  step heightmap Map.empty [(start, 0)]

step :: Heightmap -> Visited -> [(Coords, Int)] -> Visited
step _ visited [] = visited
step heightmap visited ((coords, inSteps):xs)
  | coords `Map.member` visited = step heightmap visited xs  -- been there, nothing to do
  | otherwise = step heightmap (Map.insert coords inSteps visited) (xs ++ (newReachableFrom heightmap visited coords |> map (, inSteps + 1)))

newReachableFrom :: Heightmap -> Visited -> Coords -> [Coords]
newReachableFrom heightmap visited coords = do
  let heightAt = at heightmap
  let possibilities = [(fst coords, snd coords -1), (fst coords, snd coords + 1), (fst coords -1, snd coords), (fst coords + 1, snd coords)]
  filter (\t -> (fst t >= 0) && (fst t < length (head heightmap)) && (snd t >= 0) && (snd t < length heightmap)) possibilities |> -- within area
    filter (\c -> not (c `Map.member` visited)) |>  -- not yet visited
    filter (\c -> heightAt c <= heightAt coords + 1) -- not too high

at :: Heightmap -> Coords -> Int
at heightmap coords = heightmap !! snd coords !! fst coords

day12_2 :: [String] -> String
day12_2 _ = "Nothing yet"
