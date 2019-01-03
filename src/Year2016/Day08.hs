{-# LANGUAGE ScopedTypeVariables #-}

module Year2016.Day08 where

import Debug.Trace

data Command =
      Rectangle Int Int    -- ^ Draw a rectangle starting from 0,0
    | RotateRow Int Int    -- ^ Rotate row with coordinate y by distance d in positive x direction
    | RotateColumn Int Int -- ^ Rotate column with coordinate x by distance d in positive y direction
    deriving (Show)

-- * Solvers
solveA :: String -> Int
solveA input = 0

solveB :: String -> Int
solveB input = 0

-- * Parsing

parseInput :: String -> [Command]
parseInput = fmap parseCommand . lines

parseCommand :: String -> Command
parseCommand line =
    case words line of
        ["rect", args] ->
            Rectangle (read $ takeWhile (/= 'x') args) (read $ tail $ dropWhile (/= 'x') args)
        ["rotate", "row", ydat, _, d] ->
            RotateRow (read $ drop 2 ydat) (read d)
        ["rotate", "column", xdat, _, d] ->
            RotateColumn (read $ drop 2 xdat) (read d)
        _ -> error ("invalid command: '" ++ line ++ "'")

-- * Implementation

