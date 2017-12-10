module Pos (Pos, addP, scaleP, manhattan) where

type Pos = (Int, Int)

addP :: Pos -> Pos -> Pos
addP (ax, ay) (bx, by) = (ax + bx, ay + by)

scaleP :: Int -> Pos -> Pos
scaleP s (x, y) = (s*x, s*y)

manhattan :: Pos -> Pos -> Int
manhattan (ax, ay) (bx, by) = abs (ax-bx) + abs (ay-by)
