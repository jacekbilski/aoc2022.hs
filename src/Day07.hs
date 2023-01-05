module Day07 (day07_1, day07_2) where

--import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as Map
import Flow

--data File = RegularFile String Int | Directory String [File]
--type Directory = Map String [RegularFile]  -- name, files
type Directory = (String, [RegularFile])
type RegularFile = (String, Int)  -- name, size

day07_1 :: [String] -> Int
day07_1 input = do
  let root = buildDirectoryTree input
  findDirs root |> filter (\d -> size d <= 100000) |> map size |> sum

buildDirectoryTree :: [String] -> Directory
--buildDirectoryTree = doBuildDirectoryTree (Map.fromList [("/", [])]) []
buildDirectoryTree = doBuildDirectoryTree ("/", []) []

doBuildDirectoryTree :: Directory -> [Directory] -> [String] -> Directory -- root -> current path -> input -> final directory tree
doBuildDirectoryTree root _ [] = root
doBuildDirectoryTree root path (x:xs)
  | x == "$ cd /" = do  -- I do hope this is just the first command and happens just once
    let newRoot = ("/", [])
    doBuildDirectoryTree newRoot [newRoot] xs
  | x == "$ ls" = doBuildDirectoryTree root path xs -- nothing to do
--  | x == "$ cd .." = doBuildDirectoryTree root (tail path) xs
  | otherwise = do
    let y = words x
    if head y == "dir"
    then error "Unsupported yet"  -- not yet supported
    else do -- should be pattern: "size fileName"
      let fs = read (head y) :: Int
      let filename = y !! 1
      let file = (filename, fs)
      let newRoot = ("/", [file])
      doBuildDirectoryTree newRoot [newRoot] xs

--  | Just dirName <- stripPrefix "$ cd " x = doBuildDirectoryTree root ((head path) !! dirName) xs
--  | otherwise = error "Unsupported yet"
--doBuildDirectoryTree _ _ _ = ("/", Just ("x", 123))

findDirs :: Directory -> [Directory]
findDirs dir = [dir]
--findDirs (Directory _ [files]) = []
--findDirs (RegularFile _ _) = []

size :: Directory -> Int
size (_, files) = map snd files |> sum
--size dir = Map.elems dir |> concat |> map snd |> sum
--size (Directory name [files]) = 0
--size (RegularFile _ s) = s

day07_2 :: [String] -> Int
day07_2 _ = -1
