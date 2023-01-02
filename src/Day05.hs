module Day05 (day05_1, day05_2) where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

day05_1 :: [String] -> String
day05_1 input = do
  let stacks = load_stacks input
  stacks |> Map.map head |> Map.elems

load_stacks :: [String] -> Map Int [Char]
load_stacks input = do
  let separator_idx = zip [0..] input |> filter (\i -> snd i == "") |> head |> fst
  let stack_input_indexes = zip [0..] (input !! (separator_idx - 1)) |> filter (\c -> snd c /= ' ') |> map (\t -> fst t)
  let stacks_input = take (separator_idx - 1) input
  map (\s -> (s, take_col stacks_input s |> filter (\c -> c /= ' '))) stack_input_indexes |> Map.fromList

take_col :: [[Char]] -> Int -> [Char]
take_col tab col = map (\r -> r !! col) tab

day05_2 :: [String] -> String
day05_2 input = undefined
