module Day2.Part1 where

import Data.Range ( inRange, (+=+) )
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
verifyPassword mn mx char password = inRange range count                                   
    where count = countCharInPassword char password
          range = mn +=+ mx

countCharInPassword :: Eq a => a -> [a] -> Int
countCharInPassword char = length . filter (== char)                                  
