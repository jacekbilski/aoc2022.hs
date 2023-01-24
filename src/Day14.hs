module Day14 (day14_1, day14_2) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Flow

type Slice = Map Int (Set Int)

type Coords = (Int, Int)

type StopFunction = Slice -> Coords -> Bool

day14_1 :: [String] -> Int
day14_1 input = do
  let slice = parseInput input
  let abyssLevel = Map.keys slice |> maximum
  let stopFunction = \_ c -> snd c > abyssLevel
  pourSand stopFunction slice

parseInput :: [String] -> Slice
parseInput input = map parsePath input |> foldl mergeSlices Map.empty

parsePath :: String -> Slice
parsePath input = do
  let coords = splitOn " -> " input |> map toCoords
  buildSlice coords

toCoords :: String -> Coords
toCoords input = do
  let list = splitOn "," input |> map read
  (head list, list !! 1)

buildSlice :: [Coords] -> Slice
buildSlice coords = doBuildSlice coords Map.empty

doBuildSlice :: [Coords] -> Slice -> Slice
doBuildSlice [_] slice = slice
doBuildSlice (c1 : c2 : cx) slice
  | fst c1 == fst c2 = doBuildSlice (c2 : cx) (map (fst c1,) [(min (snd c1) (snd c2)) .. (max (snd c1) (snd c2))] |> foldl addCoords slice)
  | otherwise = doBuildSlice (c2 : cx) (map (,snd c1) [(min (fst c1) (fst c2)) .. (max (fst c1) (fst c2))] |> foldl addCoords slice)
doBuildSlice [] _ = error "doBuildSlice cannot work with an empty list of coords"

addCoords :: Slice -> Coords -> Slice
addCoords slice coords
  | Map.member (snd coords) slice = Map.adjust (Set.insert (fst coords)) (snd coords) slice
  | otherwise = Map.insert (snd coords) (Set.singleton (fst coords)) slice

mergeSlices :: Slice -> Slice -> Slice
mergeSlices into from = doMergeSlices into (Map.toList from)

doMergeSlices :: Slice -> [(Int, Set Int)] -> Slice
doMergeSlices into [] = into
doMergeSlices into (x : xs)
  | Map.member (fst x) into = doMergeSlices (Map.adjust (Set.union (snd x)) (fst x) into) xs
  | otherwise = doMergeSlices (uncurry Map.insert x into) xs

pourSand :: StopFunction -> Slice -> Int
pourSand stopFunction = doPourSand stopFunction 0

doPourSand :: StopFunction -> Int -> Slice -> Int
doPourSand stopFunction units slice = do
  let (newSlice, stop) = doPourSandUnit stopFunction slice (500, 0)
  if stop then units else doPourSand stopFunction (units + 1) newSlice

doPourSandUnit :: StopFunction -> Slice -> Coords -> (Slice, Bool) -- Bool - True = sand is falling into abyss, False = sand comes to rest
doPourSandUnit stopFunction slice sandUnit
  | stopFunction slice sandUnit = (slice, True) -- done, stopFunction kicked in
  | not (Map.member (snd sandUnit + 1) slice) = doPourSandUnit stopFunction slice (fst sandUnit, snd sandUnit + 1) -- no rocks on this level, fall down
  | not (Set.member (fst sandUnit) (slice Map.! (snd sandUnit + 1))) = doPourSandUnit stopFunction slice (fst sandUnit, snd sandUnit + 1)
  | not (Set.member (fst sandUnit - 1) (slice Map.! (snd sandUnit + 1))) = doPourSandUnit stopFunction slice (fst sandUnit - 1, snd sandUnit + 1)
  | not (Set.member (fst sandUnit + 1) (slice Map.! (snd sandUnit + 1))) = doPourSandUnit stopFunction slice (fst sandUnit + 1, snd sandUnit + 1)
  | otherwise = (addCoords slice sandUnit, False) -- sand at rest, stopFunction did not yet kick in

day14_2 :: [String] -> Int
day14_2 input = do
  let slice = parseInput input
  let floorLevel = (Map.keys slice |> maximum) + 2
  let sliceWithFloor = mergeSlices slice (buildSlice [(500 - floorLevel - 2, floorLevel), (500 + floorLevel + 2, floorLevel)])
  let stopFunction = \s _ -> Map.member 0 s && Set.member 500 (s Map.! 0)
  pourSand stopFunction sliceWithFloor
