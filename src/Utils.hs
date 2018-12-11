{-|

Some utility functions for testing as well as for general data manipulation.

-}

module Utils
    (
    -- * Lists
      sortOn
    , nubOn
    , groupOn
    , sortUniq

    -- * Testing
    , getInput
    , test
    , slow
    , runSlowTests
    ) where

import Paths_adventofcode (getDataFileName)
import Text.Printf
import Test.Hspec (Spec, SpecWith, shouldBe, it)

import Data.Function
import Data.List (sort, group, sortBy, groupBy, nubBy)

-- | Set to False to skip tests which have manually been marked as 'slow'.
runSlowTests :: Bool
runSlowTests = False

-- | Skip the following test if 'runSlowTests' is true
slow :: SpecWith a -> SpecWith a
slow test = if runSlowTests
    then test
    else return ()

-- | Fetches the input string from the correct directory and file for this task
getInput :: Int  -- ^ Year
         -> Int  -- ^ Day
         -> IO String
getInput year day = do
    filename <- getDataFileName $ printf "inputs/Year%04d/%02d.in" year day
    readFile filename

-- | Helper function to add a Hspec testcase
test :: (Show b, Eq b) => String    -- ^ name of the subtask
                       -> (a -> b)  -- ^ solver function
                       -> a         -- ^ input value
                       -> b         -- ^ expected output value
                       -> Spec
test partName solver input result =
    it ("solves " ++ partName) $ do
        solver input `shouldBe` result

-- | Sorts a list by applying a transformation to the elements
-- and comparing the results. Useful for e.g. sorting on the
-- second elements of a list of pairs.
--
-- >>> sortOn snd [(1,2),(4,1)]
-- [(4,1),(1,2)]
sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

-- | Like 'sortOn', but for grouping elements
--
-- >>> groupOn snd [(1,1),(2,1),(2,2)]
-- [[(1,1),(2,1)],[(2,2)]]
groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

-- | Like 'sortOn', but for removing duplicates
--
-- >>> nubOn snd [(1,1),(2,1),(2,2)]
-- [(1,1),(2,2)]
nubOn :: (Eq b) => (a -> b) -> [a] -> [a]
nubOn f = nubBy ((==) `on` f)

-- | Replicates the bash commands @sort | uniq@, i.e. it returns
-- the sorted input with duplicates removed.
sortUniq :: (Ord a, Eq a) => [a] -> [a]
sortUniq = map head . group . sort
