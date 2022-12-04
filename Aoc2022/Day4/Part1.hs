module Aoc2022.Day4.Part1 where

import Data.List.Split

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: [[Int]] -> Int
calculate xs = length $ filter harrie xs

harrie :: [Int] -> Bool
harrie (x1:x2:y1:y2:[])
  | x1 >= y1 && x2 <= y2 = True
  | y1 >= x1 && y2 <= x2 = True
  | otherwise = False

parse :: String -> [[Int]]
parse x = map (map readInt) $ map (splitOneOf ",-") $ lines x

readInt :: String -> Int 
readInt = read