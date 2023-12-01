module Aoc2023.Day1.Part1 where

import Data.Char

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ sum . map parse (lines input)

parse :: String -> Int
parse = read . (\digits -> [head digits, last digits]) . filter isDigit
