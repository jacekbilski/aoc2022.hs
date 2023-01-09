module Day07 (day07_1, day07_2) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

data File = RegularFile Int | Directory (Map String File)

day07_1 :: [String] -> Int
day07_1 input = findDirs (buildDirectoryTree input) |> filter (\d -> size d <= 100000) |> map size |> sum

buildDirectoryTree :: [String] -> File
--buildDirectoryTree = doBuildDirectoryTree (Map.fromList [("/", [])]) []
buildDirectoryTree = doBuildDirectoryTree (Directory Map.empty)

doBuildDirectoryTree :: File -> [String] -> File -- root -> current path -> input -> final directory tree
doBuildDirectoryTree (RegularFile _) _ = error "/ cannot be a regular file"
doBuildDirectoryTree root [] = root
doBuildDirectoryTree (Directory root) (x:xs)
  | x == "$ cd /" =  -- I do hope this is just the first command and happens just once
    doBuildDirectoryTree (Directory root) xs
  | x == "$ ls" = doBuildDirectoryTree (Directory root) xs -- nothing to do
--  | x == "$ cd .." = doBuildDirectoryTree root (tail path) xs
  | otherwise = do
    let y = words x
    if head y == "dir"
      then error "Unsupported yet"  -- not yet supported
      else do -- should be pattern: "size fileName"
        let fs = read (head y) :: Int
        let filename = y !! 1
        let file = RegularFile fs
        doBuildDirectoryTree (Directory (Map.insert filename file root)) xs

findDirs :: File -> [File]
findDirs dir = [dir]
--findDirs (Directory _ [files]) = []
--findDirs (RegularFile _ _) = []

size :: File -> Int
size (Directory dir) = Map.elems dir |> map size |> sum
size (RegularFile s) = s

day07_2 :: [String] -> Int
day07_2 _ = -1
