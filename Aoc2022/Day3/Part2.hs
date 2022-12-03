module Aoc2022.Day3.Part2 where

import Data.Char

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: [(String, String, String)] -> Int
calculate = sum . map (scores . overlap)

scores :: Char -> Int
scores x
  | elem x ['a' .. 'z'] = num - 96
  | otherwise = num - 38
  where
    num = ord x

overlap :: (String, String, String) -> Char
overlap (x : xs, ys, zs)
  | elem x ys && elem x zs = x
  | otherwise = overlap (xs, ys, zs)

parse :: String -> [(String, String, String)]
parse = vertupel . lines

vertupel :: [String] -> [(String, String, String)]
vertupel [] = []
vertupel (x:y:z:zs) = (x, y, z): vertupel zs 
