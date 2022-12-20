module Day02 (day02_1, day02_2) where

day02_1 :: String -> Integer
day02_1 input = foldl1 (+) (map score (lines input))

score :: String -> Integer
score round = case round of
-- outcome + shape I selected
  "A X" -> 3 + 1
  "A Y" -> 6 + 2
  "A Z" -> 0 + 3
  "B X" -> 0 + 1
  "B Y" -> 3 + 2
  "B Z" -> 6 + 3
  "C X" -> 6 + 1
  "C Y" -> 0 + 2
  "C Z" -> 3 + 3
  _ -> error "invalid round"

day02_2 :: String -> Integer
day02_2 input = foldl1 (+) (map scoreForOutcome (lines input))

scoreForOutcome :: String -> Integer
scoreForOutcome round = 0
