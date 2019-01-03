{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day13 where

import Pos
import Data.Array (Array, (!))
import qualified Array as A
import Control.Monad

-- * Types

-- | A piece of train track
data Tile = NS  -- ^ North South
          | EW  -- ^ East West
          | NW  -- ^ North West turn (\)
          | NE  -- ^ North East turn (/)
          | Intersection
          | Empty
          deriving (Eq)

instance Show Tile where
    show NS = "|"
    show EW = "-"
    show NW = "\\"
    show NE = "/"
    show Intersection = "+"
    show Empty = " "

instance Read Tile where
    readsPrec _ (x:xs) = [(parseTile x, xs)]

data Dir = North | South | West | East deriving (Eq, Ord)
instance Show Dir where
    show North = "^"
    show South = "v"
    show West = "<"
    show East = ">"

type Player = (Pos, Dir)

type Map = (Array Pos Tile, [Player])

-- * Solutions

solveA :: String -> String
solveA input = "99,99"

solveB :: String -> Integer
solveB input = 0

-- * Parsing

-- | /Note/: The parser assumes all lines to be of equal length, i.e. padded
-- with spaces at the end.
parseInput :: String -> Map
parseInput input = (arr, players)
    where
        arr = A.array ((0,0), (length $ head $ lines input, length $ lines input)) $ (\(y,line) -> zip (x,y) $ fmap parseTile) =<< (zip [0..] $ lines input)
        players = parsePlayers input

parsePlayers :: String -> [Player]
parsePlayers input = do
    (c, pos) <- concat $ (\(line, y) -> zip line [(y,x) | x <- [0..]]) <$> zip (lines input) [0..]
    guard $ c `elem` "^v<>"
    return (pos, parseDirection c)

parseDirection :: Char -> Dir
parseDirection '>' = East
parseDirection 'v' = South
parseDirection '^' = North
parseDirection '<' = West

-- | Parses wagons as the underlying track tiles.
parseTile :: Char -> Tile
parseTile '\\' = NW
parseTile '/'  = NE
parseTile '+'  = Intersection
parseTile ' '  = Empty
parseTile x
    | x `elem` "|^v" = NS
    | x `elem` "-><" = EW

-- * Implementation

-- | Calculate the resulting direction for a wagon entering
-- a tile with a given direction.
updateDirection :: Tile -> Dir -> Dir
updateDirection NW North = West
updateDirection NW East  = South
updateDirection NW South = East
updateDirection NW West  = North
updateDirection NE North = East
updateDirection NE West  = South
updateDirection NE South = West
updateDirection NE East  = North
updateDirection _ d = d

