module AoC2022 (day01) where

import System.IO
import Control.Exception

day01 :: IO ()
day01 = bracket (openFile "inputs/day01.input1" ReadMode) hClose
                (\h -> do contents <- hGetContents h
                          sumCalories contents)

sumCalories :: String -> IO ()
sumCalories input = do
  let lns = lines input
  putStrLn "The first 10 lines:"
  mapM_ putStrLn (take 10 lns)
