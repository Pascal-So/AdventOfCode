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

solveA :: [Layer] -> Int
solveA = sum . map (cost 0)

solveB :: [Layer] -> Int
solveB layers =
    head $ filter (\n -> not $ any (caught n) layers) [0..]

main :: IO ()
main = do
    input <- parseLayers <$> getContents
    print $ solveB input