module Year2017.Day13 (solveA, solveB) where

type Layer = (Int, Int)

caught :: Int -> Layer -> Bool
caught starttime (depth, range) =
    arrival `mod` period == 0
    where
        period = range * 2 - 2
        arrival = depth + starttime

cost :: Int -> Layer -> Int
cost starttime layer@(depth, range) =
    if caught starttime layer then
        depth * range
    else
        0

parseLayers :: String -> [Layer]
parseLayers =
    map (\[a,b] -> (a,b)) . map (map (read . takeWhile (/= ':')) . words) . lines

solveA :: String -> Int
solveA = sum . map (cost 0) . parseLayers

solveB :: String -> Int
solveB input =
    head $ filter (\n -> not $ any (caught n) layers) [0..]
    where
        layers = parseLayers input
