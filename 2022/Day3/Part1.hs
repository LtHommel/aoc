module Aoc2022.Day3.Part1 where

import Data.Char

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: [(String, String)] -> Int
calculate = sum . map (scores . overlap)

scores :: Char -> Int
scores x
  | elem x ['a' .. 'z'] = num - 96
  | otherwise = num - 38
  where
    num = ord x

overlap :: (String, String) -> Char
overlap (x : xs, ys)
  | elem x ys = x
  | otherwise = overlap (xs, ys)

parse :: String -> [(String, String)]
parse x = map halves $ lines x

halves :: String -> (String, String)
halves x = splitAt ((length x) `div` 2) x