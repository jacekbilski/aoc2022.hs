module Day11 (day11_1, day11_2) where

import Data.List (stripPrefix)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Flow
import Prelude hiding (id)

data Monkey = Monkey
  { id :: Int,
    items :: [Int],
    operation :: Int -> Int,
    test :: Int -> Bool,
    ifTrue :: Int,
    ifFalse :: Int
  }

instance Show Monkey where
  show monkey = "Monkey, id: " ++ show (id monkey) ++ ", items: " ++ show (items monkey)

type Monkeys = Map Int Monkey

day11_1 :: [String] -> IO Int
day11_1 input = do
  let monkeys = loadMonkeys input
  print (monkeys Map.! 0)
  return 0

loadMonkeys :: [String] -> Monkeys
loadMonkeys input = chunksOf 7 input |> map loadMonkey |> map (\m -> (id m, m)) |> Map.fromList

loadMonkey :: [String] -> Monkey
loadMonkey input = do
  let iId = head input |> stripPrefix "Monkey " |> fromJust |> init |> read
  let iItems = input !! 1 |> stripPrefix "  Starting items: " |> fromJust |> filter (/= ',') |> words |> map read
  let iOperation = input !! 2 |> stripPrefix "  Operation: new = " |> fromJust |> loadOperation
  let iTest = (\x -> x `rem` (input !! 3 |> stripPrefix "  Test: divisible by " |> fromJust |> read) == 0)
  let iIfTrue = input !! 4 |> stripPrefix "    If true: throw to monkey " |> fromJust |> read
  let iIfFalse = input !! 5 |> stripPrefix "    If false: throw to monkey " |> fromJust |> read
  Monkey {id = iId, items = iItems, operation = iOperation, test = iTest, ifTrue = iIfTrue, ifFalse = iIfFalse}

loadOperation :: String -> Int -> Int
loadOperation input = do
  let brokenDownInput = words input
  let f = loadFunction (brokenDownInput !! 1)
  let operand = brokenDownInput !! 2
  case operand of
    "old" -> (\x -> f x x)
    _ -> f (read operand)

loadFunction :: String -> Int -> Int -> Int
loadFunction "+" = (+)
loadFunction "*" = (*)
loadFunction f = error ("Function " ++ f ++ " unsupported")

--round :: Monkeys -> Monkeys
--round monkeys = do

day11_2 :: [String] -> Int
day11_2 input = undefined
