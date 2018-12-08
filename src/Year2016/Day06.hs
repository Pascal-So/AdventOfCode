module Year2016.Day06 (solveA, solveB) where

import Data.List (sort, group, transpose)
import Utils (sortOn)

sortByFrequency :: (Ord a) => [a] -> [a]
sortByFrequency = fmap head . sortOn length . group . sort

-- | I guess after the hashing it's nice to have a task that
-- consists just of a transpose and sort.
solveA :: String -> String
solveA = fmap (last . sortByFrequency) . transpose . lines

-- | Also I was going to move more of this stuff out to separate
-- functions seeing how it's almost the same task twice, but
-- on the other hand it fits on a single line so I guess it's fine.
solveB :: String -> String
solveB = fmap (head . sortByFrequency) . transpose . lines
