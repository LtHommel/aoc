module Aoc2022.Day16.Part1 where

import Data.List
import Data.Char

main :: IO ()
main = do
  input <- readFile "example.txt"
  print  $ parse $ input

-- parse :: String -> [(String, Int, [String])]
parse xs = map (parse' . words) $ lines xs

-- parse' :: [String] -> (String, Int, [String])
parse' xs = (valve,rate, leads)
  where
    valve = xs!!1
    rate = readInt $ filter isDigit (xs!!4)
    leads = leads xs

readInt :: String -> Int
readInt = read