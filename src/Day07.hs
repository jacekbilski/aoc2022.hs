module Day07 (day07_1, day07_2) where

import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Flow

totalDiskSpace = 70000000 :: Int
requiredFreeSpace = 30000000 :: Int

data File = RegularFile Int | Directory (Map String File) deriving (Show)

day07_1 :: [String] -> Int
day07_1 input = findDirs (buildDirectoryTree input) |> map size |> filter (<= 100000) |> sum

buildDirectoryTree :: [String] -> File
buildDirectoryTree = doBuildDirectoryTree (Directory Map.empty) []

doBuildDirectoryTree :: File -> [String] -> [String] -> File -- root -> current path -> input -> final directory tree
doBuildDirectoryTree (RegularFile _) _ _ = error "/ cannot be a regular file"
doBuildDirectoryTree root _ [] = root
doBuildDirectoryTree (Directory root) path (x:xs)
  | x == "$ cd /" =  -- I do hope this is just the first command and happens just once
    doBuildDirectoryTree (Directory root) [] xs
  | x == "$ ls" = doBuildDirectoryTree (Directory root) path xs -- nothing to do
  | "$ cd " `isPrefixOf` x = do
    let dirName = drop 5 x
    if dirName == ".."
      then doBuildDirectoryTree (Directory root) (tail path) xs
      else doBuildDirectoryTree (Directory root) (dirName : path) xs
  | otherwise = do
    let y = words x
    if head y == "dir"
      then do
        let filename = y !! 1
        doBuildDirectoryTree (addFile filename (Directory Map.empty) path (Directory root)) path xs
      else do -- should be pattern: "size fileName"
        let fs = read (head y) :: Int
        let filename = y !! 1
        let file = RegularFile fs
        doBuildDirectoryTree (addFile filename file path (Directory root)) path xs

addFile :: String -> File -> [String] -> File -> File -- filename -> actual file -> path -> current root -> new root
addFile filename file [] (Directory root) = Directory (Map.insert filename file root)
addFile filename file (currDir:restPath) (Directory root) = do
  let atPath = getFile (currDir:restPath) (Directory root)
  case atPath of
    Directory dirAtPath -> do
      let withNew = Directory (Map.insert filename file dirAtPath)
      addFile currDir withNew restPath (Directory root)

getFile :: [String] -> File -> File -- path -> dir -> found file
getFile path = doGetFile (reverse path)

doGetFile :: [String] -> File -> File -- path -> dir -> found file
doGetFile [] file = file
doGetFile (x:xs) (Directory dir) = doGetFile xs (dir Map.! x)

findDirs :: File -> [File]
findDirs (Directory dir) = Directory dir : (Map.elems dir |> concatMap findDirs)
findDirs (RegularFile _) = []

size :: File -> Int
size (Directory dir) = Map.elems dir |> map size |> sum
size (RegularFile s) = s

day07_2 :: [String] -> Int
day07_2 input = do
  let root = buildDirectoryTree input
  let freeSpace = totalDiskSpace - size root
  let spaceToRecover = requiredFreeSpace - freeSpace
  findDirs root |> map size |> filter (>= spaceToRecover) |> sort |> head
