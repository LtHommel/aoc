module Aoc2022.Day2.Part1 where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ calculate $ parse input

calculate :: [(Char,Char)] -> Int
calculate = sum . map scores

scores :: (Char, Char) -> Int
scores (theirs,mine) 
    | theirs == 'A' && mine == 'X' = 4 -- 1 for rock + 3 for draw
    | theirs == 'A' && mine == 'Y' = 8 -- 2 for paper + 6 for win
    | theirs == 'A' && mine == 'Z' = 3 -- 3 for scissors + 0 for lose
    | theirs == 'B' && mine == 'X' = 1 -- 1 for rock + 0 for lose
    | theirs == 'B' && mine == 'Y' = 5 -- 2 for paper + 3 for draw
    | theirs == 'B' && mine == 'Z' = 9 -- 3 for scissors + 6 for win
    | theirs == 'C' && mine == 'X' = 7 -- 1 for rock + 6 for win
    | theirs == 'C' && mine == 'Y' = 2 -- 2 for paper + 0 for lose
    | theirs == 'C' && mine == 'Z' = 6 -- 3 for scissors + 3 for draw

parse :: String -> [(Char, Char)]
parse x = map vertupel $ lines x

vertupel :: [a] -> (a, a)
vertupel x = (x!!0, x!!2)


-- A for Rock, B for Paper, and C for Scissors.
-- X for Rock, Y for Paper, and Z for Scissors
-- 1 for Rock, 2 for Paper, and 3 for Scissors
-- 0 if you lost, 3 if the round was a draw, and 6 if you won
-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. 
-- If both players choose the same shape, the round instead ends in a draw    
