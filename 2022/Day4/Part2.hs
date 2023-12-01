module Aoc2022.Day4.Part2 where

import Data.List.Split
import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: [[Int]] -> Int
calculate xs = length $ filter hasIntersection xs

hasIntersection :: [Int] -> Bool
hasIntersection (x1:x2:y1:y2:[]) = not . null $ intersect [x1..x2] [y1..y2]

parse :: String -> [[Int]]
parse x = map (map readInt) $ map (splitOneOf ",-") $ lines x

readInt :: String -> Int 
readInt = read