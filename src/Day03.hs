module Day03 (day03_1, day03_2) where

import Flow

-- split into compartments
-- find common element
-- calculate priority
-- sum priorities

type Compartment = String
type Rucksack = (Compartment, Compartment)

day03_1 :: String -> Int
--day03_1 input = lines input |> intoRucksack |> findCommonType |> calculatePriority |> sum
day03_1 input = do
  let processOneRucksack = intoRucksack .> findCommonType .> calculatePriority
  sum (map processOneRucksack (lines input))

intoRucksack :: String -> Rucksack
intoRucksack items = splitAt (div (length items) 2) items

findCommonType :: Rucksack -> Char
findCommonType rucksack = 'a'

calculatePriority :: Char -> Integer
calculatePriority typ = 1

calculatePriority :: Char -> Int
calculatePriority typ = if typ < 'a' then (ord typ) - 38 else (ord typ) - 96

day03_2 :: String -> Int
day03_2 input = undefined
