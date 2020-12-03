module Day2.Part2 where

import Data.List ( foldl' )
import Data.List.Split ( splitOn )

main :: IO()
main = do
    input <- readFile "input.txt"
    print . length . filter (==True). map compute . lines $ input

compute :: [Char] -> Bool
compute string = verifyPassword (read (listedInput !! 0) :: Int) (read (listedInput !! 1) :: Int) (head (listedInput !! 2)) (listedInput !! 4)
    where listedInput = splitOnAnyOf ["-", " ", ":"] string

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

verifyPassword :: Int -> Int -> Char -> [Char] -> Bool
verifyPassword pos1 pos2 char password = length (filter (==char) chars) == 1
    where chars = getCharsAtPosition pos1 pos2 password

getCharsAtPosition :: Int -> Int -> String -> String
getCharsAtPosition pos1 pos2 password = [chr1, chr2]
    where chr1 = password !! (pos1 - 1)
          chr2 = password !! (pos2 - 1)
