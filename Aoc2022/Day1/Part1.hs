module Aoc2022.Day1.Part1 where

import Data.List.Split

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ calculate $ parse input

calculate :: [[Int]] -> Int
calculate = maximum . map sum

parse :: String -> [[Int]]
parse x = map (map readInt) $ splitOn [""] $ lines x

readInt :: String -> Int 
readInt = read
