{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day06 (solveA, solveB) where

import Pos
import Debug.Trace
import Data.Tuple
import Data.Function
import Data.List (sort, transpose, minimumBy, sortBy, group, groupBy)
import Data.Graph.Inductive (Gr, Node, Edge, mkUGraph)
import qualified Data.Graph.Inductive.Query.BFS as BFS

coordRange :: [Pos] -> (Pos, Pos)
coordRange points = ((minimum xs - border, minimum ys - border), (maximum xs + border + 1, maximum ys + border + 1))
    where
        xs = map fst points
        ys = map snd points
        border = 500

readInput :: String -> [Pos]
readInput = map readPos . lines
    where
        readPos str = (x,y)
            where
                [wordX, wordY] = words str
                x = read $ init wordX
                y = read wordY

gridGraph :: (Pos, Pos) -> Gr () ()
gridGraph ((minx, miny), (maxx, maxy)) = mkUGraph nodes (edges ++ map swap edges)
    where
        nodes = [y * (maxx - minx) + x | x <- [minx .. maxx - 1], y <- [miny .. maxy - 1]]
        edges = [(y * (maxx - minx) + x, y * (maxx - minx) + x + 1) | x <- [minx .. maxx - 2], y <- [miny .. maxy - 1]]
             ++ [(y * (maxx - minx) + x, (y + 1) * (maxx - minx) + x) | x <- [minx .. maxx - 1], y <- [miny .. maxy - 2]]

minimumOn f = minimumBy (compare `on` f)

sortOn f = sortBy (compare `on` f)

getCoords :: Int -> Int -> Pos
getCoords width p = (x,y)
    where
        x = p `mod` width
        y = p `div` width

onBorder :: (Pos, Pos) -> Pos -> Bool
onBorder ((minx, miny), (maxx, maxy)) (x,y) =
    x == minx || x == maxx - 1 || y == miny || y == maxy - 1

solveA :: String -> Int
solveA input = maximum sizes
    where
        points = readInput input :: [Pos]
        range = coordRange points :: (Pos, Pos)
        minx = fst $ fst range :: Int
        maxx = fst $ snd range :: Int
        startnodes :: [Node]
        startnodes = map (\(x,y) -> y * (maxx - minx) + x) points
        levels :: [[(Node, Int)]]
        levels = transpose $ map (sort . flip BFS.level (gridGraph range)) startnodes

        validClosestPoints :: [(Pos, Pos)]
        validClosestPoints = do
            dists <- levels
            let sorted = sortOn (snd . fst) $ zip dists points
            if snd (fst (sorted!!0)) == snd (fst (sorted!!1)) then
                []
            else
                return (getCoords (maxx - minx) $ fst $ fst $ head sorted, snd $ head sorted)

        borderPoints :: [Pos]
        borderPoints = map head $ group $ sort $ map snd $ filter (\(p,_) -> onBorder range p) validClosestPoints

        sizes :: [Int]
        sizes = map length $ groupBy ((==) `on` snd) $ filter (\(_,source) -> not (source `elem` borderPoints)) validClosestPoints


-- > 452
-- > 2457

solveB :: String -> Int
solveB input = 0
