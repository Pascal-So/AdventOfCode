module Year2017.Day12 where

import Data.Graph.Inductive

solveA :: Gr n e -> Int
solveA = length . dfs [0]

solveB :: Gr n e -> Int
solveB = length . components

parseGraph :: String -> Gr () ()
parseGraph str = mkUGraph nodes edges
    where
        list = map (map read . words . filter (`elem` (' ' : ['0' .. '9']))) . lines $ str
        nodes = map head list
        edges = concat $ map (\(x:xs) -> map ((,) x) xs) list

main = do
    input <- parseGraph <$> getContents
    print $ solveB input