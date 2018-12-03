module Year2017.Day01 (solveA, solveB) where

import Data.Char (ord)

shiftBy :: Int -> [a] -> [a]
shiftBy n as =
    end ++ begin
    where
        begin = take n as
        end = drop n as

shiftMatchSum :: Int -> [Int] -> Int
shiftMatchSum shift nums =
    sum $ map fst $ filter (uncurry (==)) $ zipped
    where
        zipped = zip nums $ shiftBy shift nums

solveN :: Int -> String -> Int
solveN n = shiftMatchSum n . map (\c -> ord c - ord '0') . filter (`elem` ['0' .. '9'])

solveA :: String -> Int
solveA = solveN 1

solveB :: String -> Int
solveB input =
    solveN n input
    where
        n = length input `div` 2

