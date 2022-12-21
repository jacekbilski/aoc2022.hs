module Day03 (day03_1, day03_2) where

import Flow
import Data.List
import Data.List.Split
import Data.Char

-- split into compartments
-- find common element
-- calculate priority
-- sum priorities

type Compartment = String
type Rucksack = (Compartment, Compartment)

day03_1 :: String -> Int
day03_1 input = do
  let processOneRucksack = intoRucksack .> findCommonType .> calculatePriority
  sum (map processOneRucksack (lines input))

intoRucksack :: String -> Rucksack
intoRucksack items = splitAt (div (length items) 2) items

findCommonType :: Rucksack -> Char
findCommonType rucksack = intersect (rucksack |> fst |> sort) (rucksack |> snd |> sort) |> head

calculatePriority :: Char -> Int
calculatePriority typ = if typ < 'a' then (ord typ) - 38 else (ord typ) - 96

-- group by 3
-- intersect
-- calculate priority
-- sum

day03_2 :: String -> Int
day03_2 input = do
  let processOneGroup = findGroupType .> calculatePriority
  sum (map processOneGroup ((lines input) |> chunksOf 3))

findGroupType :: [String] -> Char
findGroupType [a,b,c] = intersect a b |> intersect c |> head
