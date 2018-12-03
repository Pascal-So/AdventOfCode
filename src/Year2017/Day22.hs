module Year2017.Day22 (solveA, solveB) where

import TwoWayList (TWL)
import qualified TwoWayList as TWL
import Debug.Trace
import Control.Monad

data State = Clean | Weakened | Infected | Flagged deriving (Eq)

instance Show State where
    show Clean    = "."
    show Weakened = "W"
    show Infected = "#"
    show Flagged  = "F"

type Grid = TWL (TWL State)

data Subtask = TaskA | TaskB deriving (Eq)

type Pos = (Int, Int)

type SimState = (Grid, Pos, Pos)

rotRight :: Pos -> Pos
rotRight (x,y) =
    (y, -x)

rotLeft :: Pos -> Pos
rotLeft (x,y) =
    (-y, x)

turn :: State -> Pos -> Pos
turn Clean    = rotLeft
turn Weakened = id
turn Infected = rotRight
turn Flagged  = rotRight . rotRight

move :: Pos -> Pos -> Pos
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

initSimState :: Grid -> SimState
initSimState grid =
    (grid, (0,0), (0,1))

lookupGrid :: Grid -> Pos -> State
lookupGrid grid (x,y) =
    TWL.index (TWL.index grid y) x

adjustGrid :: Grid -> Pos -> (State -> State) -> Grid
adjustGrid grid (x,y) f =
    TWL.adjust (TWL.adjust f x) y grid

modifyState :: Subtask -> State -> State
modifyState TaskA Clean    = Infected
modifyState TaskB Clean    = Weakened
modifyState _     Weakened = Infected
modifyState TaskA Infected = Clean
modifyState TaskB Infected = Flagged
modifyState _     Flagged  = Clean

step :: Subtask -> SimState -> SimState
step subtask (g, p, d) =
    (newg, move p newd, newd)
    where
        state = lookupGrid g p
        newd = turn state d
        newg = adjustGrid g p (modifyState subtask)

center :: TWL a -> TWL a
center lst =
    TWL.shift (TWL.length lst `div` 2) lst

readGrid :: String -> Grid
readGrid =
    wrapIn empty . map (wrapIn Clean .  map (\c -> if c == '#' then Infected else Clean)) . reverse . lines
    where
        empty = TWL.fromLists (repeat Clean) (repeat Clean)
        wrapIn wrap content =
            TWL.fromLists (l ++ repeat wrap) (r ++ repeat wrap)
            where
                (l,r) = TWL.toLists $ center $ TWL.fromList content

solveA :: String -> Int
solveA input =
    length . filter (== Clean) . take 10000 . map (\(g,p,_) -> lookupGrid g p) $ iterate (step TaskA) $ initSimState grid
    where
        grid = readGrid input

solveB :: String -> Int
solveB input =
    length . filter (== Weakened) . take 10000000 . map (\(g,p,_) -> lookupGrid g p) $ iterate (step TaskB) $ initSimState grid
    where
        grid = readGrid input
