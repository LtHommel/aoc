module Aoc2022.Day8.Part1 where

import Data.List
import Data.Char

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse $ input

calculate :: [[Int]] -> Int
calculate trees = length . filter (id) . concat $ zipWith (zipWith (||)) rows cols
  where
    rows = map eenRij' trees
    cols = transpose (map eenRij' (transpose trees))

eenRij' :: [Int] -> [Bool]
eenRij' rij = zipWith (||) eneKantOp andereKantOp
  where
    eneKantOp = eenRij (-1) rij
    andereKantOp = reverse $ eenRij (-1) (reverse rij)

eenRij :: Int -> [Int] -> [Bool]
eenRij _ [] = []
eenRij hoogste (boom:rij)
  | boom > hoogste = True:(eenRij boom rij)
  | otherwise = False: (eenRij hoogste rij)

parse :: String -> [[Int]]
parse input = map (map digitToInt) $ lines input