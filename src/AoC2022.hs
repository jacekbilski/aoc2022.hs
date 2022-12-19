module AoC2022 (day01) where

import System.IO
import Control.Exception

day01 :: IO ()
day01 = bracket (openFile "inputs/day01.input1" ReadMode) hClose
                (\h -> do contents <- hGetContents h
                          putStrLn "The first 100 chars:"
                          putStrLn (take 100 contents))
