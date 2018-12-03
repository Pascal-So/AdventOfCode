module Year2017.Day10 (solveA, solveB) where

import KnotHash

readCommaList :: String -> [Int]
readCommaList str =
    map read $ words $ map f str
    where
        f ',' = ' '
        f c = c

solveA :: String -> Int
solveA str =
    let
        lst = sparseHash $ readCommaList str
    in
        (lst !! 0) * (lst !! 1)

solveB :: String -> String
solveB = hash
