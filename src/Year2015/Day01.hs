module Year2015.Day01 (solveA, solveB) where

import Data.List

moveBy :: Char -> Int
moveBy '(' =  1
moveBy ')' = -1
moveBy  c  =  0

solveA :: String -> Int
solveA = sum . map moveBy

solveB :: String -> Maybe Int
solveB lst =
    elemIndex (-1) pref
    where pref = scanl (+) 0 $ map moveBy lst


main = do
    input <- getLine
    print $ solveB input
