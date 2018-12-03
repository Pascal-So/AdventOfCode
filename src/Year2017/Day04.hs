module Year2017.Day04 (solveA, solveB) where

import Data.List
import Control.Applicative

validPhrase :: (Eq a, Ord a) => [a] -> Bool
validPhrase xs =
    distinct $ sort xs
    where
        distinct (a:b:xs) =
            if a == b then
                False
            else
                distinct (b:xs)
        distinct _ = True

solveA :: String -> Int
solveA =
    length . filter validPhrase . map words . lines

solveB :: String -> Int
solveB =
    length . filter (validPhrase . map sort) . map words . lines
