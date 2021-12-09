module Day4.Part1 where

import Data.List.Split (splitOn)
import Data.Strings ( Str(strSplit) )

type Passport = [(String,String)]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . validate . parse $ input

parse :: String -> [Passport]
parse = map (createPassport . replace) . split

split :: [Char] -> [[Char]]
split = splitOn "\n\n" 

replace :: String -> String
replace = map (\x -> if x == '\n' then ' ' else x)

createPassport :: String -> Passport
createPassport fs = [strSplit ":" fields | fields <- splitOn " " fs]

validate :: [Passport] -> Int
validate = foldr ((+) . validatePassport) 0


validatePassport :: Passport -> Int
validatePassport passport | length passport == 8                                 = 1
                          | length passport == 7 && notElem "cid" passportFields = 1
                          | otherwise                                            = 0
    where passportFields = map fst passport