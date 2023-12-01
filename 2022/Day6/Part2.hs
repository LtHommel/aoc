module Aoc2022.Day6.Part2 where

import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate 14 $ input

calculate :: Int -> String -> Int
calculate i cs
  | length ( nub (take 14 cs)) /= 14 = calculate (i + 1) (tail cs)
  | otherwise = i
