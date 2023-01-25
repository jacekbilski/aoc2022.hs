module Day16 (day16_1, day16_2) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow
import Text.Regex.TDFA

type Flow = Int
type Valve = String
type Cave = Map Valve [Valve]
type Flows = Map Valve Flow

day16_1 :: [String] -> Int
day16_1 input = do
  let (cave, flows) = parseInput input
  length cave

parseInput :: [String] -> (Cave, Flows)
parseInput input = do
  let merge = \(cave, flows) (valve, flow, tunnels) -> (Map.insert valve tunnels cave, Map.insert valve flow flows)
  map parseValve input |> -- [(valve, flow, tunnels)]
    foldl merge (Map.empty, Map.empty)

parseValve :: String -> (Valve, Flow, [Valve])
parseValve input = do
  let pat = "Valve ([A-Z]{2}) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-Z, ]+)"
  let parsed = (input =~ pat :: [[String]]) |> head
  let valve = parsed !! 1
  let flow :: Flow = read (parsed !! 2)
  let tunnels = splitOn ", " (parsed !! 3)
  (valve, flow, tunnels)

day16_2 :: [String] -> Int
day16_2 input = do
  undefined
