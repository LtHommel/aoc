module Aoc2022.Day8.Part2 where

import Data.List
import Data.Char

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse $ input

calculate :: [[Int]] -> Int
calculate forest = maximum [ scenicScore forest (c,r) | c <- [0..(length (forest!!0)-1)], r <- [0..((length forest)-1)]]

-- scenicScore :: [[Int]] -> (Int,Int) -> Int
-- scenicScore forest (c,r) = east * west * north * south 
--   where
--     treeHeight = forest!!r!!c
--     treeRow = forest!!r
--     treeColumn = transpose forest !! c
--     east =  length . takeUntil (>= treeHeight) $ drop (c+1) treeRow
--     west =  length . takeUntil (>= treeHeight) $ reverse $ take (c-1) treeRow
--     north = length . takeUntil (>= treeHeight) $ drop (r+1) treeColumn
--     south = length . takeUntil (>= treeHeight) $ reverse $ take (r-1) treeColumn

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore grid (l, c) = fromLeft * fromTop * fromRight * fromBottom
    where 
        height     = grid !! l !! c
        line       = grid !! l
        column     = transpose grid !! c
        numVisible = length . (\(f, s) -> f ++ if null s then [] else [head s]) . span (< height)
        fromLeft   = numVisible . reverse $ take c line
        fromTop    = numVisible . reverse $ take l column
        fromRight  = numVisible $ drop (c + 1) line
        fromBottom = numVisible $ drop (l + 1) column

parse :: String -> [[Int]]
parse input = map (map digitToInt) $ lines input

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil p (x:xs)
  | p x            = [x]
  | otherwise      = x : takeUntil p xs