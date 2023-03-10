module Day11 (day11_1, day11_2) where

import Data.List (sort,stripPrefix)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Flow
import Prelude hiding (id, round)

type Item = Int
type MonkeyId = Int
type Items = (Map MonkeyId [Item], Map MonkeyId Int)  -- (actual items, number of inspections)
type ReliefFunction = Item -> Item
type NormalizeFunction = Item -> Item

data Monkey = Monkey
  { id :: MonkeyId,
    operation :: Item -> Item,
    test :: Item -> Bool,
    ifTrue :: MonkeyId,
    ifFalse :: MonkeyId
  }

type Monkeys = Map MonkeyId Monkey

day11_1 :: [String] -> Int
day11_1 input = do
  let (monkeys, items) = loadMonkeys input
  let relief = (`div` 3)
  let normalize = \x -> x
  let itemsAfter = rounds monkeys relief normalize 20 items
  snd itemsAfter |> Map.elems |> sort |> reverse |> take 2 |> product

loadMonkeys :: [String] -> (Monkeys, Items)
loadMonkeys input = do
  let (monkeys, items) = chunksOf 7 input |> map loadMonkey |> map (\(m, i) -> ((id m, m), (id m, i))) |> unzip
  let monkeysMap = Map.fromList monkeys
  let inspections = map (, 0) (Map.keys monkeysMap) |> Map.fromList
  (monkeysMap, (Map.fromList items, inspections))

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

rounds :: Monkeys -> ReliefFunction -> NormalizeFunction -> Int -> Items -> Items
rounds _ _ _ 0 items = items
rounds monkeys relief normalize roundNo items = rounds monkeys relief normalize (roundNo-1) (round monkeys relief normalize items)

round :: Monkeys -> ReliefFunction -> NormalizeFunction -> Items -> Items
round monkeys relief normalize = do
  let monkeyIds = Map.keys monkeys
  turns monkeys relief normalize monkeyIds

turns :: Monkeys -> ReliefFunction -> NormalizeFunction -> [MonkeyId] -> Items -> Items
turns _ _ _ [] items = items
turns monkeys relief normalize (id:ids) items = do
  if null (fst items Map.! id)
    then turns monkeys relief normalize ids items
    else do
      let item = head (fst items Map.! id)
      let (newMonkey, newItem) = inspectAndThrow relief (monkeys Map.! id) item
      let normalizedNewItem = normalize newItem
      let newItems = Map.adjust tail id (fst items) |> Map.adjust ([normalizedNewItem] ++ ) newMonkey
      turns monkeys relief normalize (id:ids) (newItems, Map.adjust (+1) id (snd items))

inspectAndThrow :: ReliefFunction -> Monkey -> Item -> (MonkeyId, Item)
inspectAndThrow relief monkey item = do
  let withWorryLevel = operation monkey item
  let afterRelief = relief withWorryLevel
  if test monkey afterRelief
    then (ifTrue monkey, afterRelief)
    else (ifFalse monkey, afterRelief)

day11_2 :: [String] -> Int
day11_2 input = do
  let (monkeys, items) = loadMonkeys input
  let relief = \x -> x
  let leastCommonMultiple = map (stripPrefix "  Test: divisible by ") input |> filter isJust |> map fromJust |> map read |> product
  let normalize = (`rem` leastCommonMultiple)
  let itemsAfter = rounds monkeys relief normalize 10000 items
  snd itemsAfter |> Map.elems |> sort |> reverse |> take 2 |> product
