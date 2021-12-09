--      ____
--     /....\
--    /......\
--   /........\
--  /..........\
-- |............|
-- |............|
-- |............|
-- |............|
--  \........../
--   \......../
--    \....../
--     \..../
--      ----


module Infi where

teShoppenLapjes :: [Int] -> Int
teShoppenLapjes = sum . map (lapjesPerZak . benodigdeZak)

lapjesPerZak :: Int -> Int
lapjesPerZak n = n * 8

benodigdeZak :: Int -> Int
benodigdeZak = rekenUit 0 

rekenUit :: Int -> Int -> Int
rekenUit n aantalInwoners | heleDing n > aantalInwoners = n
                          | otherwise                   = rekenUit (n+1) aantalInwoners

heleDing :: Int -> Int
heleDing n = inhoudMidden n + inhoudTopEnOnderkant n

inhoudTopEnOnderkant :: Int -> Int
inhoudTopEnOnderkant n = n * (lengteMiddenrij n -2  + n)
   
inhoudMidden :: Int -> Int
inhoudMidden n = n * lengteMiddenrij n

lengteMiddenrij :: Int -> Int
lengteMiddenrij n = n + 2 * n
