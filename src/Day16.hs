module Day16 (day16_1, day16_2) where

import Data.List.Split (splitOn)
import Data.Maybe (isJust, listToMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow
import Text.Regex.TDFA

type Flow = Int
type Valve = String
type Distance = Int
type Tunnel = (Valve, Distance)
type Tunnels = Map Valve [Tunnel]
type Flows = Map Valve Flow
type Cave = (Tunnels, Flows)

day16_1 :: [String] -> Int
day16_1 input = do
  let cave = parseInput input
  let zeroFlowValves = Map.toList (snd cave) |> filter (\(v, f) -> f == 0 && v /= "AA") |> map fst
  let reducedCave = foldl removeZeroFlowValve cave zeroFlowValves
--  error (show (fst reducedCave))
--  error (show (map (\v -> length (fst cave Map.! v)) (Map.keys (fst cave)) |> sum))
  let maxPressure = openValves reducedCave
  maxPressure

parseInput :: [String] -> Cave
parseInput input = do
  let merge = \(tunnels, flows) (valve, flow, newTunnels) -> (Map.insert valve (map (, 1) newTunnels) tunnels, Map.insert valve flow flows)
  map parseValve input |> foldl merge (Map.empty, Map.empty)

parseValve :: String -> (Valve, Flow, [Valve])
parseValve input = do
  let pat = "Valve ([A-Z]{2}) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-Z, ]+)"
  let parsed = (input =~ pat :: [[String]]) |> head
  let valve = parsed !! 1
  let flow :: Flow = read (parsed !! 2)
  let tunnels = splitOn ", " (parsed !! 3)
  (valve, flow, tunnels)

removeZeroFlowValve :: Cave -> Valve -> Cave
removeZeroFlowValve (tunnels, flows) valveToRemove = do
  let tunnelsTo :: [Tunnel] = Map.toList tunnels |> map (\(from, tunnel) -> (from, filter (\(v, _) -> v == valveToRemove) tunnel |> listToMaybe)) |> filter (isJust . snd) |> map (\(from, Just (_, distance)) -> (from, distance))
  let tunnelsFrom = tunnels Map.! valveToRemove
  let newTunnels = foldl (\acc tunnel -> replaceTunnels acc valveToRemove tunnel tunnelsTo) tunnels tunnelsFrom
  (Map.delete valveToRemove newTunnels, Map.delete valveToRemove flows)

replaceTunnels :: Tunnels -> Valve -> Tunnel -> [Tunnel] -> Tunnels
replaceTunnels tunnels valveToRemove (fromValve, fromDistance) toValves =
  Map.adjust (\ts -> filter (\(v, _) -> v /= valveToRemove) ts ++ (map (\(v, d) -> (v, d + fromDistance)) toValves |> filter (\(v, _) -> v /= fromValve))) fromValve tunnels

-- idea: depth first
-- at every valve calculate max pressure for all possible routes starting here with and without opening this valve
-- take max and return
openValves :: Cave -> Int
openValves cave = doOpenValves cave "AA" 20 []

doOpenValves :: Cave -> Valve -> Int -> [Valve] -> Int
-- cave -> current position -> minutes left -> opened valves -> max pressure possible to release
doOpenValves (tunnels, flows) currentValve minutesLeft openedValves
  | minutesLeft <= 0 = 0
  | otherwise = do
    let maxWithoutOpening = map (\(v, d) -> doOpenValves (tunnels, flows) v (minutesLeft - d) openedValves) (tunnels Map.! currentValve) |> maximum
    let maxWithOpening = if (currentValve `elem` openedValves) || (flows Map.! currentValve == 0)
        then 0 -- current valve is already open or its flow is 0, maxWithoutOpening already includes best available solution
        else do
          let pressureReleasedHere = (minutesLeft - 1) * (flows Map.! currentValve)
          let furtherValves = map (\(v, d) -> doOpenValves (tunnels, flows) v (minutesLeft - d - 1) (currentValve:openedValves)) (tunnels Map.! currentValve) |> maximum
          pressureReleasedHere + furtherValves
    max maxWithoutOpening maxWithOpening

day16_2 :: [String] -> Int
day16_2 input = do
  undefined
