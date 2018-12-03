module Year2017.Day10 where

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

main = do
    input <- getLine
    print $ solveB input
