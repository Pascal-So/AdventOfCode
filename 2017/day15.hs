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

solveA :: Int -> Int -> Int
solveA startA startB = countMatching . take 40000000 $ pairs
    where
        pairs = zip (iterate generatorA startA) (iterate generatorB startB)

solveB :: Int -> Int -> Int
solveB startA startB =
    countMatching . take 5000000 $ zip numsA numsB
    where
        numsA = filter (\n -> n `mod` 4 == 0) $ iterate generatorA startA
        numsB = filter (\n -> n `mod` 8 == 0) $ iterate generatorB startB   

main = print $ solveB 873 583