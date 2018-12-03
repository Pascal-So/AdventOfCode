module Year2016.Day03 (solveA, solveB) where

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

readInput :: String -> [[Int]]
readInput = map (map read . words) . lines

solveA :: String -> Int
solveA =
    length . filter isTriangle . readInput

solveB :: String -> Int
solveB =
    length . filter isTriangle . groupsOf 3 . concat . transpose . readInput
