module Aoc2021.Day2.Part2 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . calculate 0 0 0 . lines $ input

calculate :: Int -> Int -> Int -> [String] -> Int
calculate pos depth _ [] = pos * depth
calculate pos depth aim (x:xs) | direction == "forward" = calculate (pos + distance) (depth + (aim * distance)) aim xs
                           | direction == "up" = calculate pos depth (aim - distance) xs
                           | direction == "down" = calculate pos depth (aim + distance) xs
                           | otherwise = 0
    where direction = head (words x)
          distance = read (last (words x))