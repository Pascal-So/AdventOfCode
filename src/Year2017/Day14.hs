module Year2017.Day14 where

import KnotHash (hash)
import Data.Maybe
import Data.List
import Data.Char
import Numeric
import Data.Graph.Inductive (Gr, Node, Edge, mkUGraph, components)

type Grid = [[Bool]]

rows :: String -> [String]
rows str =
    map (\n -> str ++ "-" ++ show n) [0..127]

hexToBinary :: String -> String
hexToBinary =
    concat . map charToHex
    where
        charToHex c =
            leftPad 4 '0' . flip (showIntAtBase 2 (\d -> chr $ ord '0' + d)) "" . fst . head . readHex $ [c]
        leftPad len c str = replicate (len - length str) c ++ str

getGrid :: String -> Grid
getGrid =
        map (map (== '1') . hexToBinary) . map hash . rows

getNodes :: Grid -> [Node]
getNodes grid =
    map fst . filter snd . concat . map (\(y, row) ->
        map (\(x, val) -> (y * (length row) + x, val)) $ zip [0..] row
    ) $ zip [0..] grid

getEdges :: Grid -> [Edge]
getEdges grid =
    horizontal ++ vertical
    where
        rowEdges = zipWith (&&)
        rowLength = length $ head grid
        toPairs addY addX grid =
            concat $ map catMaybes $ map (\(y,row) ->
                map (\(x,v) ->
                    if v
                        then Just (y * rowLength + x, (y + addY) * rowLength + x + addX)
                        else Nothing
                    ) $ zip [0..] row
            ) $ zip [0..] grid
        horizontal = toPairs 0 1 $ map (\r -> rowEdges r (tail r)) grid
        vertical = toPairs 1 0 $ transpose $ map (\r -> rowEdges r (tail r)) $ transpose grid

toGraph :: Grid -> Gr () ()
toGraph grid = mkUGraph (getNodes grid) (getEdges grid)

solveA :: Grid -> Int
solveA =
    sum . map (length . filter id)

solveB :: Grid -> Int
solveB = length . components . toGraph

main :: IO ()
main = do
    input <- getGrid <$> getLine
    print $ solveB input