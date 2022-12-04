module Aoc2022.Day3.Part2 where

import Data.Char

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: [(String, String, String)] -> Int
calculate = sum . map (scores . intersect)

scores :: Char -> Int
scores x
  | elem x ['a' .. 'z'] = num - 96
  | otherwise = num - 38
  where
    num = ord x

intersect :: (String, String, String) -> Char
intersect (x : xs, ys, zs)
  | elem x ys && elem x zs = x
  | otherwise = intersect (xs, ys, zs)

parse :: String -> [(String, String, String)]
parse = chunked . lines

chunked :: [String] -> [(String, String, String)]
chunked [] = []
chunked (x:y:z:zs) = (x, y, z): chunked zs 
