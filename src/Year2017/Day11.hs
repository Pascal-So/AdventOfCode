module Year2017.Day11 (solveA, solveB) where

import Data.List

data Direction = North | Northeast | Southeast | South | Southwest | Northwest deriving (Show)
type Pos = (Int, Int)

readDir :: String -> Direction
readDir "n" = North
readDir "ne" = Northeast
readDir "se" = Southeast
readDir "s" = South
readDir "sw" = Southwest
readDir "nw" = Northwest

move :: Pos -> Direction -> Pos
move (x,y) North     = (x,y+1)
move (x,y) Northeast = (x+1,y)
move (x,y) Southeast = (x+1,y-1)
move (x,y) South     = (x,y-1)
move (x,y) Southwest = (x-1,y)
move (x,y) Northwest = (x-1,y+1)

dist :: Pos -> Pos -> Int
dist (xa, ya) (xb, yb) =
    if sgn dx == sgn dy then
        dx + dy
    else
        max dx dy
    where
        sgn x = compare x 0
        dx = (xb - xa)
        dy = (yb - ya)


readDirs :: String -> [Direction]
readDirs =
    map readDir . words . map (replace ',' ' ')
    where
        replace a b c
            | a == c    = b
            | otherwise = c

solveA :: String -> Int
solveA =
    dist (0,0) . foldl' move (0,0) . readDirs

solveB :: String -> Int
solveB =
    maximum . map (dist (0,0)) . scanl move (0,0) . readDirs
