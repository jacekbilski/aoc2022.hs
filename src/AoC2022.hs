module AoC2022 (solve, day01_1, day01_2) where

import System.IO
import Control.Exception

import Day01

solve :: FilePath -> (String -> Integer) -> IO Integer -- function per day, input file name -> result
solve fileName day = do input <- (readInput fileName)
                        return (day input)

readInput :: FilePath -> IO String
readInput fileName = bracket (openFile fileName ReadMode) hClose hGetContents'
