module Aoc2022.Day7.Part2 where

import Data.List

data File  = Dir String [File]
           | File String Int

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse $ input

calculate :: File -> Int
calculate fs = head . sort . filter (>= sizeNeeded) $ sizes
  where
    sizes = dirSizes fs
    sizeNeeded = 30000000 - (70000000 - (maximum sizes))

dirSizes :: File -> [Int]
dirSizes (File _ _) = []
dirSizes (Dir name files) = fileSize (Dir name files) : foldl (\a d -> a ++ dirSizes d) [] files

fileSize :: File -> Int
fileSize (File _ size) = size
fileSize (Dir _ files) = sum $ map fileSize files

parse :: String -> File
parse input = snd . flip parseFileSystem (Dir "/" []) $ drop 1 $ lines input

parseFileSystem :: [String] -> File -> ([String], File)
parseFileSystem [] fileSystem = ([], fileSystem)
parseFileSystem (line : lines) (Dir currentDir files)
  | isPrefixOf "$ cd .." line = (lines, Dir currentDir files) -- return subtree
  | isPrefixOf "$ cd " line = parseFileSystem (fst subTree) (Dir currentDir (snd subTree : files)) -- maak een subtree van nieuwe dir
  | isPrefixOf "$ ls" line = parseFileSystem lines (Dir currentDir files) -- doe lekker niks
  | isPrefixOf "dir " line = parseFileSystem lines (Dir currentDir files) -- doe ook niks, dir wordt toegevoegd bij cd
  | otherwise = parseFileSystem lines (Dir currentDir (newFile : files)) -- lees file
  where
    splitted = words line
    newFile = File (splitted !! 1) (read (splitted !! 0) :: Int)
    subTree = parseFileSystem lines (Dir (drop 5 line) [])
