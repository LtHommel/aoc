module Day3.Part2 where

type Strategy = (Int, Int)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print . compute . lines $ input

strategies :: [Strategy]
strategies = [(1, 1), (3,1), (5,1), (7,1), (1,2)]

compute :: [String] -> Int
compute = compute' strategies

compute' :: [Strategy] -> [String] -> Int
compute' [] _             = 1
compute' (s:strategies) slopes = aStrategy s slopes 0 * compute' strategies slopes

aStrategy :: Strategy -> [String] -> Int -> Int
aStrategy _ [] _     = 0
aStrategy strategy slopes i = boom + aStrategy strategy (drop down slopes) (i+right)
    where 
        right = fst strategy
        down = snd strategy
        boom | sleeen (head slopes) i == '#'  = 1
             | otherwise          = 0

sleeen :: [a] -> Int -> a
sleeen xs index = cycle xs !! index
