module AoC2022 (solve, day01_1, day01_2, day02_1, day02_2, day03_1, day03_2) where

import System.IO
import Control.Exception

import Day01
import Day02
import Day03

solve :: FilePath -> (String -> Int) -> IO Int -- function per day, input file name -> result
solve fileName day = do input <- (readInput fileName)
                        return (day input)

readInput :: FilePath -> IO String
readInput fileName = bracket (openFile fileName ReadMode) hClose hGetContents'
