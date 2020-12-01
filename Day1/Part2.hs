module Day1.Part2 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . calculate . map read . lines $ input

calculate :: [Int] -> Int
calculate input =  head [ x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]
