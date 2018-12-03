module Year2017.Day15 (solveA, solveB) where

import Numeric
import Data.Char
import Data.Bits

modulo :: Int
modulo = 2147483647

generator :: Int -> Int -> Int
generator fac n =
    n * fac `mod` modulo

generatorA :: Int -> Int
generatorA = generator 16807

generatorB :: Int -> Int
generatorB = generator 48271

lowEqual :: Int -> Int -> Bool
lowEqual a b =
    last16 a == last16 b
    where
        last16 = (.&.) 65535

countMatching :: [(Int, Int)] -> Int
countMatching = length . filter (uncurry lowEqual)

readInput :: String -> (Int, Int)
readInput str =
    (read (w!!4), read (w!!9))
    where
        w = words str

solveA :: String -> Int
solveA input = countMatching . take 40000000 $ pairs
    where
        (startA, startB) = readInput input
        pairs = zip (iterate generatorA startA) (iterate generatorB startB)

solveB :: String -> Int
solveB input =
    countMatching . take 5000000 $ zip numsA numsB
    where
        (startA, startB) = readInput input
        numsA = filter (\n -> n `mod` 4 == 0) $ iterate generatorA startA
        numsB = filter (\n -> n `mod` 8 == 0) $ iterate generatorB startB
