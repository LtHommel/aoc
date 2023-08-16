module Aoc2022.Day9.Part1 where

import Data.Char
import Data.List

type Instruction = (Char, Int)
type Position = (Int, Int)

main :: IO ()
main = do
  input <- readFile "example.txt"
  print $ calculate $ parse $ input

-- calculate :: [(Char, Int)] -> Int
calculate instructions = foldl move (0, 0) instructions

move :: Position -> Instruction -> Position 
move (x, y) (dir, n)
  | dir == 'U' = (x, y + n)
  | dir == 'D' = (x, y - n)
  | dir == 'L' = (x - n, y)
  | dir == 'R' = (x + n, y)
  | otherwise = error "ongeldige instructie"

moveTail :: Position -> Position -> Position
moveTail head tail
  | 
  where diff = sub head tail

needToMove :: Position -> Bool
needToMove tail = contains tail adjacent
 where adjacent = [(0,0),(0,1) (0,-1) (1,0) (-1,0),(1,1) (1,-1) (-1,1) (-1,1)]

sub :: Num x => (x, x) -> (x, x) -> (x, x)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

parse :: String -> [Instruction]
parse xs = map toInstruction $ map words $ lines xs

toInstruction :: [String] -> Instruction
toInstruction xs = ((head $ head xs), (readInt $ last xs))

readInt :: String -> Int
readInt = read

-- dingen die je moet weten:
-- positie head
-- positie tail
-- voorgaande posities tail

-- verschil tussen head en tail
-- (0,0) - op elkaar
-- (0,1) (0,-1) (1,0) (-1,0) direct naast elkaar
-- (1,1) (1,-1) (-1,1) (-1,1) diagonaal aangrenzend
--  