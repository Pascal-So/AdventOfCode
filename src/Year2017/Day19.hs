module Year2017.Day19 (solveA, solveB) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe

data Tile = Empty | Direction Int | Cross | Letter Char deriving (Eq)

type Board = Seq (Seq Tile)

type Pos = (Int, Int)

readTile :: Char -> Tile
readTile ' ' = Empty
readTile '-' = Direction 0
readTile '|' = Direction 1
readTile '+' = Cross
readTile  c  = Letter c

readBoard :: String -> Board
readBoard =
    Seq.fromList . map (Seq.fromList . map readTile) . lines

getStart :: Board -> Int
getStart b =
    Maybe.fromMaybe 0 $ Seq.elemIndexL (Direction 1) (Seq.index b 0)

move :: Pos -> Pos -> Pos
move (x1,y1) (x2,y2) =
    (x1+x2, y1+y2)

rotateDirRight :: Pos -> Pos
rotateDirRight (x,y) =
    (-y,x)

rotateDirLeft :: Pos -> Pos
rotateDirLeft (x,y) =
    (y,-x)

tileAt :: Board -> Pos -> Tile
tileAt b (x,y) =
    Seq.index (Seq.index b y) x

step :: Board -> (Pos, Pos) -> (Pos, Pos)
step b (pos, dir) =
    case tileAt' pos of
        Cross ->
            if tileAt' (move pos dir) /= Empty then
                (move pos dir, dir)
            else if tileAt' (move pos dirRight) /= Empty then
                (move pos dirRight, dirRight)
            else
                (move pos dirLeft, dirLeft)
        _ ->
            (move pos dir, dir)
    where
        dirRight = rotateDirRight dir
        dirLeft = rotateDirLeft dir
        tileAt' = tileAt b

walkPath :: Board -> (Pos, Pos) -> [Pos]
walkPath b init =
    takeWhile (\p -> tileAt' p /= Empty) $ map fst $ iterate (step b) init
    where
        tileAt' = tileAt b

solveA :: String -> String
solveA input =
    [x | Letter x <- (map tileAt' $ walkPath board init)]
    where
        board = readBoard input
        tileAt' = tileAt board
        init = ((getStart board, 0), (0,1))

solveB :: String -> Int
solveB input =
    length $ walkPath board init
    where
        board = readBoard input
        init = ((getStart board, 0), (0,1))
