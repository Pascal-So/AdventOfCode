module Main where

lineChecksum :: [Int] -> Int
lineChecksum line = maximum line - minimum line

checksumA :: [[Int]] -> Int
checksumA = sum . map lineChecksum


wholeDiv :: Int -> Int -> Int
wholeDiv a b = 
    if max a b `mod` min a b == 0 then
        max a b `div` min a b
    else 0

divisionSum :: [Int] -> Int
divisionSum line = 
    sum divs
    where
        divs = map (\(x:xs) -> sum $ map (wholeDiv x) xs) inits
        inits = map (\n -> drop n line) [0 .. length line - 2]

checksumB :: [[Int]] -> Int
checksumB = sum . map divisionSum



readSpreadsheet :: IO [[Int]]
readSpreadsheet =
    fmap (map (map read . words) . lines) getContents

main :: IO ()
main = readSpreadsheet >>= print . checksumB

