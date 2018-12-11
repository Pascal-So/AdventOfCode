{-|

Some utility functions for testing as well as for general data manipulation.

-}

module Utils
    (
    -- * Lists
      nubOn
    , groupOn
    , sortOn
    , sortUniq

    -- * Testing
    , getInput
    , test
    ) where

import Paths_adventofcode (getDataFileName)
import Text.Printf
import Test.Hspec (Spec, shouldBe, it)

import Data.Function
import Data.List (sort, group, sortBy, groupBy, nubBy)


getInput :: Int -> Int -> IO String
getInput year day = do
    filename <- getDataFileName $ printf "inputs/Year%04d/%02d.in" year day
    readFile filename

test :: (Show b, Eq b) => String    -- ^ name of the subtask
                       -> (a -> b)  -- ^ solver function
                       -> a         -- ^ input value
                       -> b         -- ^ expected output value
                       -> Spec
test partName solver input result =
    it ("solves " ++ partName) $ do
        solver input `shouldBe` result

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

nubOn :: (Eq b) => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)

-- | Replicates the bash commands @sort | uniq@, i.e. it returns
-- the sorted input with duplicates removed.
sortUniq :: (Ord a, Eq a) => [a] -> [a]
sortUniq = map head . group . sort