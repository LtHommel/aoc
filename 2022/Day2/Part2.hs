module Aoc2022.Day2.Part2 where

data Gesture = Rock | Paper | Scissors deriving (Eq, Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: [(Gesture, Char)] -> Int
calculate = sum . map play

play :: (Gesture, Char) -> Int
play (theirs, result)
  | result == 'X' = 0 + (score $ flop theirs)
  | result == 'Y' = 3 + (score $ theirs)
  | result == 'Z' = 6 + (score $ top theirs)

top :: Gesture -> Gesture
top g
  | g == Rock = Paper
  | g == Paper = Scissors
  | g == Scissors = Rock

flop :: Gesture -> Gesture
flop g
  | g == Rock = Scissors
  | g == Paper = Rock
  | g == Scissors = Paper

score :: Gesture -> Int
score g
  | g == Rock = 1
  | g == Paper = 2
  | g == Scissors = 3

parse :: String -> [(Gesture, Char)]
parse x = map vertupel $ lines x

vertupel :: String -> (Gesture, Char)
vertupel x = (gesture (first), third)
  where
    first = x !! 0
    third = x !! 2

gesture :: Char -> Gesture
gesture c
  | c == 'A' = Rock
  | c == 'B' = Paper
  | c == 'C' = Scissors

-- A for Rock, B for Paper, and C for Scissors.
-- X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
-- 1 for Rock, 2 for Paper, and 3 for Scissors
-- 0 if you lost, 3 if the round was a draw, and 6 if you won
-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
-- If both players choose the same shape, the round instead ends in a draw
