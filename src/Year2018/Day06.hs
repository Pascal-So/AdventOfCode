{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day06 (solveA, solveB) where

import Pos
import Utils (groupOn, sortOn)
import Control.Monad
import Debug.Trace
import Data.Tuple
import Data.Maybe
import Data.Function
import Data.List (sort, transpose, minimumBy, sortBy, group, groupBy)

type Range = (Pos, Pos)

coordRange :: [Pos] -> Range
coordRange points = ((minimum xs - border, minimum ys - border), (maximum xs + border + 1, maximum ys + border + 1))
    where
        xs = map fst points
        ys = map snd points
        border = 0

rangeWidth :: Range -> Int
rangeWidth ((minx, _), (maxx, _)) = maxx - minx

onBorder :: Range -> Pos -> Bool
onBorder ((minx, miny), (maxx, maxy)) (x,y) =
    x == minx || x == maxx - 1 || y == miny || y == maxy - 1


readInput :: String -> [Pos]
readInput = map readPos . lines
    where
        readPos str = (x,y)
            where
                [wordX, wordY] = words str
                x = read $ init wordX
                y = read wordY

closestPoint :: [Pos] -> Pos -> Maybe Pos
closestPoint points p =
    if dist closest == dist second then
        Nothing
    else
        Just closest
    where
        dist = manhattan p
        closest:second:_ = sortOn dist points

solveA :: String -> Int
solveA input = maximum sizes
    where
        points = readInput input :: [Pos]
        range = coordRange points :: Range
        ((minx, miny), (maxx, maxy)) = range

        closestPoints :: [(Pos, Maybe Pos)]
        closestPoints = do
            x <- [minx .. maxx - 1]
            y <- [miny .. maxy - 1]
            let p = (x,y)
            [(p, closestPoint points p)]

        borderPoints :: [Pos]
        borderPoints = map head $ group $ sort $ catMaybes $ map snd $ filter (\(p,_) -> onBorder range p) closestPoints

        sizes :: [Int]
        sizes = map length $ groupOn snd $ sortOn snd $ do
            (p, Just source) <- closestPoints
            guard $ not $ source `elem` borderPoints
            [(p, source)]

solveB :: String -> Int
solveB input = 0
    where
        manhattanSumLimit = 10000
        points = readInput input :: [Pos]
        xs = sort $ map fst points
        ys = sort $ map snd points
