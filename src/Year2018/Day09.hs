{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day09 where

import TwoWayList (TWL)
import qualified TwoWayList as TWL

type Circle = TWL Int
type MarbleCircle = Circle
type PlayerScores = Circle

-- * Solutions
solveA :: String -> Int
solveA input = maximum $ TWL.toList $ playGame p m
    where
        (p, m) = parseInput input

-- | There's probably a very smart way to solve this but the brute force solution only takes
-- 15 seconds on my machine and I'm calling this good enough.
solveB :: String -> Int
solveB input = maximum $ TWL.toList $ playGame p (m * 100)
    where
        (p, m) = parseInput input

-- * Parsing

-- | Expected format: "p players; last marble is wort m points"
parseInput :: String -> (Int, Int)
parseInput input = (read players, read marbles)
    where
        [players, _, _, _, _, _, marbles, _] = words input

-- * Implementation

moveRight :: Circle -> Circle
moveRight twl =
    case TWL.toLists twl of
        (ls, []) -> TWL.fromLists [head revd] (tail revd)
            where revd = reverse ls
        (ls, [r]) -> TWL.fromList $ TWL.toList twl
        _        -> TWL.shift 1 twl

moveLeft :: Circle -> Circle
moveLeft twl =
    case TWL.toLists twl of
        ([], rs) -> TWL.fromLists (tail revd) [head revd]
            where revd = reverse rs
        _        -> TWL.shift (-1) twl

insertStep :: Int -> MarbleCircle -> MarbleCircle
insertStep i = TWL.insertRight i . moveRight . moveRight

deleteStep :: MarbleCircle -> (Int, MarbleCircle)
deleteStep marbles = (TWL.index jumped 0, TWL.removeRight jumped)
    where
        jumped = iterate moveLeft marbles !! 7

step :: Int -> MarbleCircle -> PlayerScores -> (MarbleCircle, PlayerScores)
step i marbles players =
    if i `mod` 23 == 0
    then (marbles', moveRight players')
    else (insertStep i marbles, moveRight players)
    where
        (removed, marbles') = deleteStep marbles
        players' = TWL.adjust (+ (i + removed)) 0 players

initialMarbleCircle :: MarbleCircle
initialMarbleCircle = TWL.fromLists [] [0]

initialPlayerScores :: Int -> PlayerScores
initialPlayerScores p = TWL.fromList $ replicate p 0

playGame :: Int  -- ^ The number of players
         -> Int  -- ^ The number of marbles or turns
         -> PlayerScores
playGame p m =
    go 1 initialMarbleCircle (initialPlayerScores p)
    where
        go turn marbles players
            | turn <= m =
                let
                    (marbles', players') = step turn marbles players
                in
                    -- traceShow (turn, marbles, players) $
                    go (turn + 1) marbles' players'
            | otherwise = players
