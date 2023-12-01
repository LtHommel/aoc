module Aoc2022.Day6.Part1 where

import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate 4 $ input

calculate :: Int -> String -> Int
calculate i cs
  | length ( nub (take 4 cs)) /= 4 = calculate (i + 1) (tail cs)
  | otherwise = i
