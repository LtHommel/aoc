module Aoc2021.Day2.Part1 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . calculate 0 0 . lines $ input

calculate :: Int -> Int -> [String] -> Int
calculate pos depth [] = pos * depth
calculate pos depth (x:xs) | direction == "forward" = calculate (pos + distance) depth xs
                           | direction == "up" = calculate pos (depth - distance) xs
                           | direction == "down" = calculate pos (depth + distance) xs
                           | otherwise = 0
    where direction = head (words x)
          distance = read (last (words x))