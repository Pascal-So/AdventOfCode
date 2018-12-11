{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day10 where

import Pos

type Trajectory = (Pos, Pos)

-- * Solutions

exploreData :: String  -- ^ The puzzle input
            -> Int     -- ^ The time
            -> String  -- ^ The positions at that time, in a format suitable for gnuplot
exploreData input time =
    unlines $ map showPos $ constellationAt time trajectories
    where
        trajectories = parseInput input

-- * Parsing

-- | Assumed format: \"\<.*,.*\>\"
readPos :: String -> Pos
readPos str = (x, y)
    where
        x = read $ takeWhile (/= ',') $ drop 1 str
        y = read $ takeWhile (/= '>') $ tail $ dropWhile (/= ',') str

-- | Assumed format: "position=\< 52783,  52786\> velocity=\<-5, -5\>"
readTrajectory :: String -> Trajectory
readTrajectory line = (pos, vel)
    where
        pos = readPos $ take 16 $ drop  9 line
        vel = readPos $ take  8 $ drop 35 line

parseInput :: String -> [Trajectory]
parseInput = fmap readTrajectory . lines

-- * Implementation

trajectoryAt :: Int -> Trajectory -> Pos
trajectoryAt time (pos, vel) =
    addP pos $ scaleP time vel

constellationAt :: Int -> [Trajectory] -> [Pos]
constellationAt time = fmap (trajectoryAt time)

showPos :: Pos -> String
showPos (x, y) = show x ++ " " ++ show y
