module Year2016.Day03 where

import Data.List

isTriangle :: [Int] -> Bool
isTriangle t@[a,b,c] =
    x + y > z
    where
        [x,y,z] = sort t
isTriangle _ = False

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs =
    if length xs > n then
        take n xs : groupsOf n (drop n xs)
    else
        [xs]

solveA :: [[Int]] -> Int
solveA =
    length . filter isTriangle

solveB :: [[Int]] -> Int
solveB =
    solveA . groupsOf 3 . concat . transpose

main = do
    input <- map (map read . words) . lines <$> getContents
    print $ solveB input

