module Day02 (day02_1) where

day02_1 :: String -> Integer
day02_1 input = foldr (+) 0 (map score (lines input))

score :: String -> Integer
-- outcome + shape I selected
score "A X" = 3 + 1
score "A Y" = 6 + 2
score "A Z" = 0 + 3
score "B X" = 0 + 1
score "B Y" = 3 + 2
score "B Z" = 6 + 3
score "C X" = 6 + 1
score "C Y" = 0 + 2
score "C Z" = 3 + 3
score _ = 0
