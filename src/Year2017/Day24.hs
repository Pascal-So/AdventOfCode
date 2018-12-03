module Year2017.Day24 (solveA, solveB) where

import Data.Graph.Inductive hiding (getNodes)
import Data.List     as List
import Data.Maybe    as Maybe
import Data.Function

type Connector = (Int, Int)

pathStrength :: [Connector] -> Int
pathStrength =
    foldr (+) 0 . map connStrength
    where
        connStrength (a,b) = a + b

getNodes :: [Connector] -> [LNode Connector]
getNodes conn =
    concatMap cnodes $ zip [0..] conn
    where
        cnodes (idx, (a,b)) =
            [ (idx * 2    , (a,b))
            , (idx * 2 + 1, (b,a))
            ]

getEdges :: [LNode Connector] -> [UEdge]
getEdges nodes =
    concatMap findMatching nodes
    where
        findMatching (idx1, (a1, b1)) =
            map (\(idx2, _) -> (idx1, idx2, ())) filtered
            where
                filtered = filter (\(idx2, (a2, b2)) -> idx1 `div` 2 /= idx2 `div` 2 && b1 == a2) nodes

startingNodes :: [LNode Connector] -> [Node]
startingNodes nodes =
    [idx | (idx, (0,_)) <- nodes]

allPaths :: Gr a b -> Node -> [Path]
allPaths graph n =
    case match n graph of
        (Nothing, _)          -> []
        (Just context, rest)  ->
            if outdeg' context == 0 then
                [[n]]
            else
                map (n:) $ concatMap (allPaths g') $ suc' context
                where
                    otherNode = if n `mod` 2 == 0 then n+1 else n-1
                    g' = delNode otherNode rest

getConnsForPath :: Gr Connector () -> Path -> [Connector]
getConnsForPath graph nodes =
    Maybe.catMaybes $ map (lab graph) nodes

getPaths :: [Connector] -> [[Connector]]
getPaths conn =
    map (getConnsForPath graph) $ concatMap (allPaths graph) $ startingNodes nodes
    where
        nodes = getNodes conn
        edges = getEdges nodes
        graph = mkGraph nodes edges

solveA :: [Connector] -> Int
solveA conn =
    maximum $ map pathStrength $ getPaths conn

solveB :: [Connector] -> Int
solveB conn =
    pathStrength $ last $ sortOn length $ sortOn pathStrength $ getPaths conn

readConnector :: String -> Connector
readConnector str =
    (read left, read $ tail right)
    where
        (left, right) = break (=='/') str

readInput :: String -> [Connector]
readInput =
    map readConnector . lines

main = do
    input <- readInput <$> getContents
    print $ solveB input