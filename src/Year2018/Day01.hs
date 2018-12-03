module Year2018.Day01 (solveA, solveB) where

import Data.Set (Set)
import qualified Data.Set as Set

readInt :: String -> Int
readInt ('+' : num) = read num
readInt ('-' : num) = 0 - read num
readInt num = read num

solveA :: String -> Int
solveA =
    sum . map readInt . lines

scanner :: Int -> (Set Int, Int) -> (Int, Int) -> (Set Int, Int)
scanner len_input (set, sum) (idx, current) =
    if idx <= len_input + 1 then
        (Set.insert sum set, newsum)
    else
        (set, newsum)
    where newsum = sum + current

firstDupFreq :: [Int] -> Int
firstDupFreq nums =
    let scanned = scanl (scanner $ length nums) (Set.empty, 0) $ zip [1..] $ cycle nums
    in snd . head . filter (\(set, n) -> Set.member n set) $ scanned

solveB :: String -> Int
solveB =
    firstDupFreq . map readInt . lines
