module Year2016.Day02 where

import Data.List
import Data.Char
import Prelude hiding (Left, Right)

import Debug.Trace

import Pos

data Direction = Up | Down | Left | Right deriving (Show)

directionToPos :: Direction -> Pos
directionToPos Up = (0,1)
directionToPos Down = (0,-1)
directionToPos Left = (-1,0)
directionToPos Right = (1,0)

moveA :: Pos -> Direction -> Pos
moveA p d = (\(x,y) -> (limit x, limit y)) $ p `addP` directionToPos d
    where
        limit (-1) = 0
        limit   3  = 2
        limit   n  = n

moveB :: Pos -> Direction -> Pos
moveB p@(x,y) Up
    | abs(x) + y == 2 = (x,y)
    | otherwise       = (x,y+1)
moveB p@(x,y) Down
    | abs(x) - y == 2 = (x,y)
    | otherwise       = (x,y-1)
moveB p@(x,y) Left
    | -x + abs(y) == 2 = (x,y)
    | otherwise        = (x-1,y)
moveB p@(x,y) Right
    | x + abs(y) == 2 = (x,y)
    | otherwise       = (x+1,y)

charToDir :: Char -> Direction
charToDir 'U' = Up
charToDir 'D' = Down
charToDir 'L' = Left
charToDir 'R' = Right

pressKeyA :: Pos -> Char
pressKeyA (x,y) = head $ show $ x + 1 + 3 * (2 - y)

pressKeyB :: Pos -> Char
pressKeyB (_, 2) = '1'
pressKeyB (x, 1) = chr $ x + ord '3'
pressKeyB (x, 0) = chr $ x + ord '7'
pressKeyB (x,-1) = chr $ x + ord 'B'
pressKeyB (_,-2) = 'D'

solve :: (Pos -> Direction -> Pos) -> (Pos -> Char) -> Pos -> String -> String
solve moveFun keyFun startpos =
    map snd . tail . scanl runLine start . map (map charToDir) . lines
    where
        start = (startpos, 'x')
        runLine (p,_) line =
            let newP = foldl' moveFun p line
            in  (newP, keyFun newP)

solveA :: String -> String
solveA = solve moveA pressKeyA (0,0)

solveB :: String -> String
solveB = solve moveB pressKeyB (-2,0)


main = getContents >>= print . solveB