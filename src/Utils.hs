module Utils where

import Paths_adventofcode (getDataFileName)
import Text.Printf
import Test.Hspec (Spec, shouldBe, it)

import Data.Function
import Data.List (sortBy, groupBy)


getInput :: Int -> Int -> IO String
getInput year day = do
    filename <- getDataFileName $ printf "inputs/Year%04d/%02d.in" year day
    readFile filename

testPart :: (Show b, Eq b) => Int -> (a -> b) -> a -> b -> Spec
testPart nr solver input result =
    it ("solves Part " ++ (show nr)) $ do
        solver input `shouldBe` result

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)
