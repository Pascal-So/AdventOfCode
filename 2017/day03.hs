module Main where

type MemPos = (Int, Int)

levelLimits :: [Int]
levelLimits = 
    1 : [4*(x+1)*x + 1 | x <- [1..]]

level :: Int -> Int
level n =
    length $ takeWhile (< n) levelLimits

posInLevel :: Int -> Int
posInLevel 1 = 0
posInLevel n =
    n - prevLimit - 1
    where
        lev = level n
        prevLimit = levelLimits !! (lev - 1)


rot :: MemPos -> MemPos
rot (x,y) = (-y,x)

memDist :: MemPos -> MemPos -> Int
memDist (x1,y1) (x2,y2) =
    abs (x1-x2) + abs (y1-y2)

addressToPos :: Int -> MemPos
addressToPos 1 = (0, 0)
addressToPos n =
    let
        lev = level n
        pil = posInLevel n
        sideLength = lev * 2
        side = pil `div` sideLength
        offset = (pil `mod` sideLength) - lev + 1
        vec = (lev, offset)
    in
        iterate rot vec !! side

posToAddress :: MemPos -> Int
posToAddress (0,0) = 1
posToAddress (x,y) =
    levelLimits!!(lev - 1) + inLevel
    where
        lev = max (abs x) (abs y)
        dist = memDist (x,y) (lev, -lev)
        inLevel = if x + y > 0 then
            dist -- above x = -y diagonal
        else
            8 * lev - dist


solveA :: Int -> Int
solveA n =
    memDist (0,0) $ addressToPos n

getAdjacents :: MemPos -> [MemPos]
getAdjacents p@(x,y) =
    let
        adj = [(x+a,y+b) | a <- [-1..1], b <- [-1..1]]
    in
        filter (\a -> posToAddress a < posToAddress p) adj

stressTest :: [Int]
stressTest =
    1 : (map (sum . map (\i -> stressTest!!(i-1))) sumAddresses)
    where
        sumAddresses = map (map posToAddress . getAdjacents . addressToPos) [2..]


solveB :: Int -> Int
solveB n =
    head $ dropWhile (< n) stressTest

main :: IO ()
main = readLn >>= print . solveA
