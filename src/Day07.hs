module Day07 (day07_1, day07_2) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

--data File = RegularFile String Int | Directory String [File]
type Directory = Map String [RegularFile]  -- name, files
type RegularFile = (String, Int)  -- name, size

day07_1 :: [String] -> Int
day07_1 input = do
  let root = buildDirectoryTree input
  findDirs root |> filter (\d -> size d <= 100000) |> map size |> sum

buildDirectoryTree :: [String] -> Directory
buildDirectoryTree input = Map.empty

findDirs :: Directory -> [Directory]
findDirs _ = []
--findDirs (Directory _ [files]) = []
--findDirs (RegularFile _ _) = []

size :: Directory -> Int
size dir = Map.elems dir |> concat |> map snd |> sum
--size (Directory name [files]) = 0
--size (RegularFile _ s) = s

day07_2 :: [String] -> Int
day07_2 input = -1
