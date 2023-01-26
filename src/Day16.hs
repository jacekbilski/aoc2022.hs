module Day16 (day16_1, day16_2) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow
import Text.Regex.TDFA

type Flow = Int
type Valve = String
type Tunnels = Map Valve [Valve]
type Flows = Map Valve Flow
type Cave = (Tunnels, Flows)

day16_1 :: [String] -> Int
day16_1 input = do
  let cave = parseInput input
  let maxPressure = openValves cave
  maxPressure

parseInput :: [String] -> Cave
parseInput input = do
  let merge = \(tunnels, flows) (valve, flow, newTunnels) -> (Map.insert valve newTunnels tunnels, Map.insert valve flow flows)
  map parseValve input |> foldl merge (Map.empty, Map.empty)

parseValve :: String -> (Valve, Flow, [Valve])
parseValve input = do
  let pat = "Valve ([A-Z]{2}) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-Z, ]+)"
  let parsed = (input =~ pat :: [[String]]) |> head
  let valve = parsed !! 1
  let flow :: Flow = read (parsed !! 2)
  let tunnels = splitOn ", " (parsed !! 3)
  (valve, flow, tunnels)

-- idea: depth first
-- at every valve calculate max pressure for all possible routes starting here with and without opening this valve
-- take max and return
openValves :: Cave -> Int
openValves cave = doOpenValves cave "AA" 15 []

doOpenValves :: Cave -> Valve -> Int -> [Valve] -> Int
-- cave -> current position -> minutes left -> opened valves -> max pressure possible to release
doOpenValves _ _ 0 _ = 0
doOpenValves _ _ (-1) _ = 0
doOpenValves (tunnels, flows) currentValve minutesLeft openedValves = do
  let maxWithoutOpening = map (\v -> doOpenValves (tunnels, flows) v (minutesLeft - 1) openedValves) (tunnels Map.! currentValve) |> maximum
  let maxWithOpening = if (currentValve `elem` openedValves) || (flows Map.! currentValve == 0)
      then 0 -- current valve is already open or its flow is 0, maxWithoutOpening already includes best available solution
      else do
        let pressureReleasedHere = (minutesLeft - 1) * (flows Map.! currentValve)
        let furtherValves = map (\v -> doOpenValves (tunnels, flows) v (minutesLeft - 2) (currentValve:openedValves)) (tunnels Map.! currentValve) |> maximum
        pressureReleasedHere + furtherValves
  max maxWithoutOpening maxWithOpening

day16_2 :: [String] -> Int
day16_2 input = do
  undefined
