module Day07 (day07_1, day07_2) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

data File = RegularFile Int | Directory (Map String File)

day07_1 :: [String] -> Int
day07_1 input = do
  let root = buildDirectoryTree input
  let dirs = findDirs root
  let smallDirs = filter (\d -> size d <= 100000) dirs
  map size smallDirs |> sum

buildDirectoryTree :: [String] -> File
--buildDirectoryTree = doBuildDirectoryTree (Map.fromList [("/", [])]) []
buildDirectoryTree = doBuildDirectoryTree (Directory Map.empty)

doBuildDirectoryTree :: File -> [String] -> File -- root -> current path -> input -> final directory tree
doBuildDirectoryTree root [] = root
doBuildDirectoryTree root (x:xs)
  | x == "$ cd /" = do  -- I do hope this is just the first command and happens just once
    doBuildDirectoryTree root xs
  | x == "$ ls" = doBuildDirectoryTree root xs -- nothing to do
--  | x == "$ cd .." = doBuildDirectoryTree root (tail path) xs
  | otherwise = do
    let y = words x
    if head y == "dir"
      then error "Unsupported yet"  -- not yet supported
      else do -- should be pattern: "size fileName"
        let fs = read (head y) :: Int
        let filename = y !! 1
        let file = RegularFile fs
        case root of
          Directory dir -> doBuildDirectoryTree (Directory (Map.insert filename file dir)) xs
          RegularFile _ -> error "/ cannot be a regular file"
--        Directory (Map name [files]) <- root
--        doBuildDirectoryTree (Map.insert filename file root) xs

--  | Just dirName <- stripPrefix "$ cd " x = doBuildDirectoryTree root ((head path) !! dirName) xs
--  | otherwise = error "Unsupported yet"
--doBuildDirectoryTree _ _ _ = ("/", Just ("x", 123))

findDirs :: File -> [File]
findDirs dir = [dir]
--findDirs (Directory _ [files]) = []
--findDirs (RegularFile _ _) = []

size :: File -> Int
size (Directory dir) = Map.elems dir |> map size |> sum
size (RegularFile s) = s
--size (Map name files) = Map.elems dir |> sum

day07_2 :: [String] -> Int
day07_2 _ = -1
