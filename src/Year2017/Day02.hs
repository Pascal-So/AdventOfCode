module Year2017.Day02 (solveA, solveB) where

lineChecksum :: [Int] -> Int
lineChecksum line = maximum line - minimum line

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

readSpreadsheet :: String -> [[Int]]
readSpreadsheet =
    map (map read . words) . lines

solveA :: String -> Int
solveA = sum . map lineChecksum . readSpreadsheet

solveB :: String -> Int
solveB = sum . map divisionSum . readSpreadsheet
