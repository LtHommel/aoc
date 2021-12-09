module Day4.Part2 where

import Day4.Part1 (parse)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . solve . parse $ input

solve :: [Passport] -> Int
