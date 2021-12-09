module Aoc2021.Day1.Part1 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . calculate 0 . map read . lines $ input

calculate :: Int -> [Int] -> Int
calculate _ [] = -1
calculate last (x:xs) | x > last = 1 + calculate x xs
                      | otherwise = calculate x xs