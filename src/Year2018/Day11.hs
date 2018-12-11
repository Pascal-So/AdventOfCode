{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day11 where

import Data.List (transpose, elemIndex)
import Pos

-- * Solutions

solveA :: String -> String
solveA input = show x ++ "," ++ show y
    where
        grid = makeGrid $ read input
        (x,y) = maxCoord $ sumgrid 3 grid

solveB :: String -> String
solveB input = show x ++ "," ++ show y ++ "," ++ show bestN
    where
        grid = makeGrid $ read input
        bestN = snd $ maximum $ (\n -> (maximum $ maximum <$> sumgrid n grid, n)) <$> [1..300]
        (x,y) = maxCoord $ sumgrid bestN grid

-- * Implementation

-- | A maximal element together with its 1 based
-- index.
--
-- >>> maxWithIndex "aaza"
-- ('z',3)
maxWithIndex :: (Ord a) => [a] -> (a, Int)
maxWithIndex lst = (mx, idx + 1)
    where
        mx = maximum lst
        Just idx = elemIndex mx lst

-- | 1 based coordinate of a maximal element.
--
-- >>> maxCoord [[5,5],[9,5]]
-- (2,1)
maxCoord :: (Ord a) => [[a]] -> Pos
maxCoord grid = (x,y)
    where
        bestYs = maxWithIndex <$> grid
        ((val, y), x) = maxWithIndex bestYs

-- | The power level is computed as follows:
--
--   * Find the fuel cell's rack ID, which is its X coordinate plus 10.
--   * Begin with a power level of the rack ID times the Y coordinate.
--   * Increase the power level by the value of the grid serial number (your puzzle input).
--   * Set the power level to itself multiplied by the rack ID.
--   * Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
--   * Subtract 5 from the power level.
--
-- >>> getPowerLevel 71 (101,153)
-- 4
getPowerLevel :: Int  -- ^ grid serial number
              -> Pos  -- ^ coordinates
              -> Int
getPowerLevel sn (x, y) =
    ((rack * y + sn) * rack `div` 100 `mod` 10) - 5
    where
        rack = x + 10

makeGrid :: Int -> [[Int]]
makeGrid sn =
    (\x -> (\y -> getPowerLevel sn (x,y)) <$> [1..300]) <$> [1..300]

sumgrid :: Int -> [[Int]] -> [[Int]]
sumgrid n = transpose . sumcols . transpose . sumcols
    where sumcols = fmap (nSums n)

-- | \(\mathcal O(\text {length list})\). Calculates the list of sums
-- for all sublists of n elements.
--
-- >>> nSums 3 [1,1,1,5]
-- [3,7]
--
-- >>> nSums 10 [1]
-- []
--
-- >>> nSums 10 []
-- []
nSums :: Int -> [Int] -> [Int]
nSums n lst
    | length lst < n = []
    | otherwise      = scanl (+) firstsum diffs
        where
            diffs = zipWith (-) (drop n lst) lst
            firstsum = sum (take n lst)
