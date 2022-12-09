module Aoc2022.Day5.Part2 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ calculate $ parse input

calculate :: ([[Char]], [(Int, Int, Int)]) -> [Char]
calculate (stacks, instructions) = map head $ execute stacks instructions

execute :: [[Char]] -> [(Int, Int, Int)] -> [[Char]]
execute stacks [] = stacks
execute stacks (i : is) = execute (move i stacks) is

move :: (Int, Int, Int) -> [[Char]] -> [[Char]]
move (n, from, to) stacks = replace from fromStackAfter $ replace to (push popped toStack) stacks
  where
    fromStack = stacks !! (from -1)
    toStack = stacks !! (to -1)
    popped = take n fromStack
    fromStackAfter = drop n fromStack

replace :: Int -> [Char] -> [[Char]] -> [[Char]]
replace i newStack stacks = do
  let (xs, _ : ys) = splitAt (i-1) stacks
  xs ++ newStack : ys

push :: [Char] -> [Char] -> [Char]
push crates stack = crates ++ stack

parse :: String -> ([[Char]], [(Int, Int, Int)])
parse input = do
  let (stacks, instructions) = toTuple . splitOn [""] $ lines input
  (parseStacks stacks, map parseInstruction instructions)

parseStacks :: [String] -> [[Char]]
parseStacks stacks = map (\i -> filter (/= ' ') $ parseCrates stacks i) [1 .. (numberOfStacks)]
  where
    numberOfStacks = (readInt (last $ words $ last stacks))

parseCrates :: [String] -> Int -> String
parseCrates (x : []) _ = ""
parseCrates (x : xs) i = x !! i' : parseCrates xs i
  where
    i' = 4 * i - 3

parseInstruction :: String -> (Int, Int, Int)
parseInstruction x = (ints !! 1, ints !! 3, ints !! 5)
  where
    ints = map readInt $ words x -- waarom gaat deze readInt niet fout?

toTuple :: [a] -> (a, a)
toTuple xs = (xs !! 0, xs !! 1)

readInt :: String -> Int
readInt = read