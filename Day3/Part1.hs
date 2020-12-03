module Day3.Part1 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . compute 0 . lines $ input

compute :: Int -> [String] -> Int
compute _ []     = 0
compute i (x:xs) = boom + compute (i+3) xs
    where boom | sleeen x i == '#'  = 1
               | otherwise          = 0

sleeen :: [a] -> Int -> a
sleeen xs index = cycle xs !! index
