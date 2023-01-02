module Day05 (day05_1, day05_2) where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

type Stacks = Map Char [Char]
type Step = (Int, Char, Char) -- quantity, source, target
type Procedure = [Step] -> Stacks -> Stacks

day05_1 :: [String] -> String
day05_1 input = rearrange_stacks do_apply_procedure_9000 input

rearrange_stacks :: Procedure -> [String] -> String
rearrange_stacks procedure input = do
  let separator_line_index = zip [0..] input |> filter (\i -> snd i == "") |> head |> fst
  let stacks_input = take separator_line_index input
  let procedure_input = drop (separator_line_index + 1) input
  load_stacks stacks_input |> apply_procedure procedure procedure_input |> Map.map head |> Map.elems

load_stacks :: [String] -> Stacks
load_stacks input = do
  let stack_input_indexes = zip [0..] (last input) |> filter (\c -> snd c /= ' ') |> map (\t -> fst t)
  let stacks_input = init input
  map (\s -> ((last input) !! s, take_col stacks_input s |> filter (\c -> c /= ' '))) stack_input_indexes |> Map.fromList

take_col :: [[Char]] -> Int -> [Char]
take_col tab col = map (\r -> r !! col) tab

apply_procedure :: Procedure -> [String] -> Stacks -> Stacks
apply_procedure procedure input stacks = do
  let steps = parse_procedure input
  procedure steps stacks

parse_procedure :: [String] -> [Step]
parse_procedure input = input |> map words |> map (\a -> (read (a !! 1), a !! 3 !! 0, a !! 5 !! 0))

do_apply_procedure_9000 :: Procedure
do_apply_procedure_9000 [] stacks = stacks
do_apply_procedure_9000 steps stacks = do
  let step = head steps
  let (quantity, source, target) = step
  if quantity == 0
    then do
      do_apply_procedure_9000 (tail steps) stacks
    else do
      let e = stacks Map.! source |> head
      Map.adjust ([e] ++) target stacks |> Map.adjust (tail) source |> do_apply_procedure_9000 ((quantity - 1, source, target) : (tail steps))

day05_2 :: [String] -> String
day05_2 input = rearrange_stacks do_apply_procedure_9001 input

do_apply_procedure_9001 :: Procedure
do_apply_procedure_9001 [] stacks = stacks
do_apply_procedure_9001 steps stacks = do
  let step = head steps
  let (quantity, source, target) = step
  if quantity == 0
    then do
      do_apply_procedure_9001 (tail steps) stacks
    else do
      let es = stacks Map.! source |> take quantity
      Map.adjust (es ++) target stacks |> Map.adjust (drop quantity) source |> do_apply_procedure_9001 (tail steps)
