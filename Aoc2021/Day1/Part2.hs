module Aoc2021.Day1.Part2 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . calculate 0 . map read . lines $ input

calculate :: Int -> [Int] -> Int
calculate _ [] = -1
calculate last xs | a > last = 1 + calculate a (tail xs)
                  | otherwise = calculate a (tail xs)
    where a = sum (take 3 xs)