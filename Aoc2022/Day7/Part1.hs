module Aoc2022.Day7.Part1 where

import Data.List

data File
  = Dir String [File]
  | File String Int
  deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ snd . parse $ input

calculate :: File -> Int
calculate fs = sum . filter (<100000) $ dirSizes fs

dirSizes :: File -> [Int]
dirSizes (File _ _) = []
dirSizes (Dir name files) = fileSize (Dir name files) : foldl (\a d -> a ++ dirSizes d) [] files

fileSize :: File -> Int
fileSize (File _ size) = size
fileSize (Dir _ files) = sum $ map fileSize files

parse :: String -> ([String], File)
parse input = flip parseFileSystem (Dir "/" []) $ drop 1 $ lines input

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
    newFile = File (splitted !! 1) (readInt $ splitted !! 0)
    subTree = parseFileSystem lines (Dir (drop 5 line) [])

readInt :: String -> Int
readInt = read

