module Day11 (day11_1, day11_2) where

import Data.List (stripPrefix)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Flow
import Prelude hiding (id, round)

type Item = Int
type MonkeyId = Int
type Items = Map MonkeyId [Item]

data Monkey = Monkey
  { id :: MonkeyId,
    operation :: Item -> Item,
    test :: Item -> Bool,
    ifTrue :: MonkeyId,
    ifFalse :: MonkeyId
  }

type Monkeys = Map MonkeyId Monkey

day11_1 :: [String] -> String
day11_1 input = do
  let (monkeys, items) = loadMonkeys input
  let itemsAfter = rounds monkeys 20 items
  show itemsAfter
--  print (items Map.! 0)
--  return 0

loadMonkeys :: [String] -> (Monkeys, Items)
loadMonkeys input = do
  let (monkeys, items) = chunksOf 7 input |> map loadMonkey |> map (\(m, i) -> ((id m, m), (id m, i))) |> unzip
  (Map.fromList monkeys, Map.fromList items)

loadMonkey :: [String] -> (Monkey, [Item])
loadMonkey input = do
  let iId = head input |> stripPrefix "Monkey " |> fromJust |> init |> read
  let iOperation = input !! 2 |> stripPrefix "  Operation: new = " |> fromJust |> loadOperation
  let iTest = (\x -> x `rem` (input !! 3 |> stripPrefix "  Test: divisible by " |> fromJust |> read) == 0)
  let iIfTrue = input !! 4 |> stripPrefix "    If true: throw to monkey " |> fromJust |> read
  let iIfFalse = input !! 5 |> stripPrefix "    If false: throw to monkey " |> fromJust |> read
  let items = input !! 1 |> stripPrefix "  Starting items: " |> fromJust |> filter (/= ',') |> words |> map read
  (Monkey {id = iId, operation = iOperation, test = iTest, ifTrue = iIfTrue, ifFalse = iIfFalse}, items)

loadOperation :: String -> Item -> Item
loadOperation input = do
  let brokenDownInput = words input
  let f = loadFunction (brokenDownInput !! 1)
  let operand = brokenDownInput !! 2
  case operand of
    "old" -> (\x -> f x x)
    _ -> f (read operand)

loadFunction :: String -> (Item -> Item -> Item)
loadFunction "+" = (+)
loadFunction "*" = (*)
loadFunction f = error ("Function " ++ f ++ " unsupported")

rounds :: Monkeys -> Int -> Items -> Items
rounds _ 0 items = items
rounds monkeys roundNo items = rounds monkeys (roundNo-1) (round monkeys items)

round :: Monkeys -> Items -> Items
round monkeys = do
  let monkeyIds = Map.keys monkeys
  turns monkeys monkeyIds

turns :: Monkeys -> [MonkeyId] -> Items -> Items
turns _ [] items = items
turns monkeys (id:ids) items = do
  if null (items Map.! id)
    then turns monkeys ids items
    else do
      let item = head (items Map.! id)
      let (newMonkey, newItem) = inspectAndThrow (monkeys Map.! id) item
      let newItems = Map.adjust tail id items |> Map.adjust ([newItem] ++ ) newMonkey
      turns monkeys (id:ids) newItems

inspectAndThrow :: Monkey -> Item -> (MonkeyId, Item)
inspectAndThrow monkey item = do
  let withWorryLevel = operation monkey item
  let afterRelief = withWorryLevel `div` 3
  if test monkey afterRelief
    then (ifTrue monkey, afterRelief)
    else (ifFalse monkey, afterRelief)

day11_2 :: [String] -> Int
day11_2 input = undefined
