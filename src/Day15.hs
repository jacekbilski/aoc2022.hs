module Day15 (day15_1, day15_2) where

import Data.List (sortBy)
import Data.Maybe (fromJust, isJust)
import Flow
import Text.Regex.TDFA

type Coords = (Int, Int)
type Beacon = Coords
type Radius = Int -- distance to the nearest beacon
type Sensor = (Coords, Radius)
type Input = ([Sensor], [Beacon])
type Range = (Int, Int)

day15_1 :: [String] -> Int
day15_1 input = do
  let (sensors, beacons) = parseInput input
  let row = 2000000 :: Int
  let ranges = map (rangeAtRow row) sensors |> filter isJust |> map fromJust |> sortBy (\x y -> compare (fst x) (fst y)) |> foldl mergeRanges []
  let rangesWithoutBeacons = filter (\b -> snd b == row) beacons |> map fst |> foldl subtractBeacon ranges
  foldl (\acc (s, e) -> acc + (e - s + 1)) 0 rangesWithoutBeacons

parseInput :: [String] -> Input
parseInput = foldl addSensorAndBeacon ([], [])

addSensorAndBeacon :: Input -> String -> Input
addSensorAndBeacon (sensors, beacons) line = do
  let pat = "[0-9-]+"
  let nums = (getAllTextMatches (line =~ pat) :: [String]) |> map read
  let sensorCoords = (head nums, nums !! 1)
  let beacon = (nums !! 2, nums !! 3)
  let sensor = (sensorCoords, distance sensorCoords beacon)
  let newSensors = sensor : sensors
  let newBeacons = if beacon `elem` beacons then beacons else beacon : beacons
  (newSensors, newBeacons)

distance :: Coords -> Coords -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

rangeAtRow :: Int -> Sensor -> Maybe Range
rangeAtRow row ((x, y), radius) = do
  let distanceToRow = distance (x, y) (x, row)
  let radiusLeft = radius - distanceToRow
  if radiusLeft >= 0 then Just (x - radiusLeft, x + radiusLeft) else Nothing

mergeRanges :: [Range] -> Range -> [Range]  -- ranges must be sorted and non-overlapping!
mergeRanges ranges (s, e) = doMergeRanges (s, e) [] ranges

doMergeRanges :: Range -> [Range] -> [Range] -> [Range]
doMergeRanges (s, e) done [] = done ++ [(s, e)]
doMergeRanges (s, e) done ((rs, re):rest)
  | re < s = doMergeRanges (s, e) (done ++ [(rs, re)]) rest -- current completely after next
  | rs <= (e + 1) = doMergeRanges (min s rs, max e re) done rest -- current and next overlap or are adjacent -> merge the two and continue
  | otherwise = done ++ [(s, e)] ++ [(rs, re)] ++ rest  -- current completely before next, we're done

subtractBeacon :: [Range] -> Int -> [Range]  -- ranges must be sorted and non-overlapping!
subtractBeacon ranges beacon = doSubtractBeacon beacon [] ranges

doSubtractBeacon :: Int -> [Range] -> [Range] -> [Range]
doSubtractBeacon _ done [] = done
doSubtractBeacon b done ((s, e):rs)
  | b == s = if b == e then done ++ rs else done ++ [(s + 1, e)] ++ rs
  | b == e = done ++ [(s, e - 1)] ++ rs
  | b > s && b < e = done ++ [(s, b - 1), (b + 1, e)] ++ rs
  | otherwise = doSubtractBeacon b (done ++ [(s, e)]) rs

day15_2 :: [String] -> Integer
day15_2 input = do
  let maxCoord = 4000000 :: Int
  let (sensors, _) = parseInput input
  let mapRanges = \row -> map (rangeAtRow row) sensors |> filter isJust |> map fromJust |> sortBy (\x y -> compare (fst x) (fst y)) |> foldl mergeRanges [] |> cutTo (0, maxCoord)
  let ranges = map (\row -> (row, mapRanges row)) [0..maxCoord]
  let notFullyCovered = filter (\t -> (snd t |> head) /= (0, maxCoord)) ranges |> head
  let x = (head (snd notFullyCovered) |> snd) + 1
  (4000000 :: Integer) * toInteger x + toInteger (fst notFullyCovered)  -- 12856506754143 is too high

cutTo :: Range -> [Range] -> [Range]
cutTo (s, e) ranges = cutStartTo s ranges |> cutEndTo e

cutStartTo :: Int -> [Range] -> [Range]
cutStartTo s ((rs, re):rest)
  | s <= rs = (rs, re):rest
  | s <= re = (s, re):rest
  | otherwise = cutStartTo s rest
cutStartTo _ _ = error "Can't go that far"

cutEndTo :: Int -> [Range] -> [Range]
cutEndTo e = doCutEndTo e []

doCutEndTo :: Int -> [Range] -> [Range] -> [Range]
doCutEndTo e done ((rs, re):rest)
  | e < rs = done
  | e > re = doCutEndTo e (done ++ [(rs, re)]) rest
  | otherwise = done ++ [(rs, e)]
doCutEndTo _ _ _ = error "Can't go that far"
