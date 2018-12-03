module Year2017.Day12 (solveA, solveB) where

import Data.Graph.Inductive

parseGraph :: String -> Gr () ()
parseGraph str = mkUGraph nodes edges
    where
        list = map (map read . words . filter (`elem` (' ' : ['0' .. '9']))) . lines $ str
        nodes = map head list
        edges = concat $ map (\(x:xs) -> map ((,) x) xs) list

solveA :: String -> Int
solveA = length . dfs [0] . parseGraph

solveB :: String -> Int
solveB = length . components . parseGraph
