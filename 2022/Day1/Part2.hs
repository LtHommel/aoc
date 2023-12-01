module Aoc2022.Day1.Part2 where

import Data.List.Split
import Data.List

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ calculate $ parse input

calculate :: [[Int]] -> Int
calculate = sum . take 3 . reverse . sort . map sum

parse :: String -> [[Int]]
parse x = map (map read) $ splitOn [""] $ lines x