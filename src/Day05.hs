module Day05 (day05_1, day05_2) where

import Prelude hiding (lookup)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

type Stacks = Map Char [Char]
type Step = (Int, Char, Char) -- quantity, source, target
type Procedure = [Step] -> Stacks -> Stacks

day05_1 :: [String] -> String
day05_1 input = rearrangeStacks useCrateMover9000 input

rearrangeStacks :: Procedure -> [String] -> String
rearrangeStacks procedure input = do
  let separatorLineIndex = zip [0..] input |> filter (\i -> snd i == "") |> head |> fst
  let stacksInput = take separatorLineIndex input
  let procedureInput = drop (separatorLineIndex + 1) input
  loadStacks stacksInput |> applyProcedure procedure procedureInput |> Map.map head |> Map.elems

loadStacks :: [String] -> Stacks
loadStacks input = do
  let stacksInputIndexes = zip [0..] (last input) |> filter (\c -> snd c /= ' ') |> map (\t -> fst t)
  let stacksInput = init input
  map (\s -> ((last input) !! s, takeCol stacksInput s |> filter (\c -> c /= ' '))) stacksInputIndexes |> Map.fromList

takeCol :: [[Char]] -> Int -> [Char]
takeCol tab col = map (\r -> r !! col) tab

applyProcedure :: Procedure -> [String] -> Stacks -> Stacks
applyProcedure procedure input stacks = do
  let steps = parseProcedure input
  procedure steps stacks

parseProcedure :: [String] -> [Step]
parseProcedure input = input |> map words |> map (\a -> (read (a !! 1), a !! 3 !! 0, a !! 5 !! 0))

useCrateMover9000 :: Procedure
useCrateMover9000 [] stacks = stacks
useCrateMover9000 steps stacks = do
  let step = head steps
  let (quantity, source, target) = step
  if quantity == 0
    then do
      useCrateMover9000 (tail steps) stacks
    else do
      let e = stacks Map.! source |> head
      Map.adjust ([e] ++) target stacks |> Map.adjust (tail) source |> useCrateMover9000 ((quantity - 1, source, target) : (tail steps))

day05_2 :: [String] -> String
day05_2 input = rearrangeStacks useCrateMover9001 input

useCrateMover9001 :: Procedure
useCrateMover9001 [] stacks = stacks
useCrateMover9001 steps stacks = do
  let step = head steps
  let (quantity, source, target) = step
  if quantity == 0
    then do
      useCrateMover9001 (tail steps) stacks
    else do
      let es = stacks Map.! source |> take quantity
      Map.adjust (es ++) target stacks |> Map.adjust (drop quantity) source |> useCrateMover9001 (tail steps)
