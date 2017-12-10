module Main where

shiftBy :: Int -> [a] -> [a]
shiftBy n as =
    end ++ begin
    where
        begin = take n as
        end = drop n as

shiftMatchSum :: Int -> [Int] -> Int
shiftMatchSum shift nums =
    sum $ map fst $ filter (uncurry (==)) $ zipped
    where
        zipped = zip nums $ shiftBy shift nums

solveN :: Int -> String -> Int
solveN n = shiftMatchSum n . map (\c -> read [c])

main :: IO ()
main = do
    input <- getLine
    let n = length input `div` 2
    print $ solveN n input
