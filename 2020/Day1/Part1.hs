module Day1.Part1 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . head . calculate . map read . lines $ input

calculate :: [Int] -> [Int]
calculate input = [ x * y | x <- input, y <- input, x + y == 2020]
