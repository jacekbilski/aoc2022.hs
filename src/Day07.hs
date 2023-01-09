module Day07 (day07_1, day07_2) where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

data File = RegularFile Int | Directory (Map String File)

day07_1 :: [String] -> Int
day07_1 input = findDirs (buildDirectoryTree input) |> filter (\d -> size d <= 100000) |> map size |> sum

buildDirectoryTree :: [String] -> File
buildDirectoryTree = doBuildDirectoryTree (Directory Map.empty) []

doBuildDirectoryTree :: File -> [String] -> [String] -> File -- root -> current path -> input -> final directory tree
--doBuildDirectoryTree (RegularFile _) _ = error "/ cannot be a regular file"
doBuildDirectoryTree root _ [] = root
doBuildDirectoryTree (Directory root) path (x:xs)
  | x == "$ cd /" =  -- I do hope this is just the first command and happens just once
    doBuildDirectoryTree (Directory root) [] xs
  | x == "$ ls" = doBuildDirectoryTree (Directory root) path xs -- nothing to do
  | "$ cd " `isPrefixOf` x = do
    let dirName = drop 5 x
    case dirName of
      ".." -> doBuildDirectoryTree (Directory root) (tail path) xs
      otherwise -> doBuildDirectoryTree (Directory root) (dirName : path) xs
  | otherwise = do
    let y = words x
    if head y == "dir"
      then do
        let filename = y !! 1
        doBuildDirectoryTree (addFile filename (Directory Map.empty) [] (Directory root)) path xs
      else do -- should be pattern: "size fileName"
        let fs = read (head y) :: Int
        let filename = y !! 1
        let file = RegularFile fs
        doBuildDirectoryTree (addFile filename file [] (Directory root)) path xs

addFile :: String -> File -> [String] -> File -> File
addFile filename file path (Directory root) = Directory (Map.insert filename file root)

findDirs :: File -> [File]
findDirs dir = [dir]
--findDirs (Directory _ [files]) = []
--findDirs (RegularFile _ _) = []

size :: File -> Int
size (Directory dir) = Map.elems dir |> map size |> sum
size (RegularFile s) = s

day07_2 :: [String] -> Int
day07_2 _ = -1
