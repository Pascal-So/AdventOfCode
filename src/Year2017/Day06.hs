module Year2017.Day06 where

import Data.List
import Data.Maybe

mapFirstN :: (a -> a) -> Int -> [a] -> [a]
mapFirstN f n lst = map f (take n lst) ++ drop n lst

step :: [Int] -> [Int]
step lst =
    let
        m = maximum lst
        begin = takeWhile (<m) lst
        end = tail $ dropWhile (<m) lst
        totalInc = (m `div` length lst)
        endInc = min (m `mod` length lst) $ length end
        beginInc = (m `mod` length lst) - endInc
        newBegin = mapFirstN (+1) beginInc begin
        newEnd = mapFirstN (+1) endInc end
    in
        map (+ totalInc) $ newBegin ++ [0] ++ newEnd

getFirstDupPair :: (Eq a) => [a] -> (Int, Int)
getFirstDupPair lst =
    head $ catMaybes $ map dupes $ inits lst
    where
        dupes [] = Nothing
        dupes lst =
            (\i -> (i,length lst - 1)) `fmap` maybeFstIndex
            where maybeFstIndex = (elemIndex (last lst) (init lst))

solveB :: [Int] -> Int
solveB =
    (\(f,s) -> s - f) . getFirstDupPair . iterate step

solveA :: [Int] -> Int
solveA =
    snd . getFirstDupPair . iterate step

main = do
    input <- map read . words <$> getLine
    print $ solveB input