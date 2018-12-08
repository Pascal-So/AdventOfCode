module Year2015Spec (spec) where

import Test.Hspec (Spec, describe, runIO, parallel)
import qualified Year2015.Day01 as D01
import Utils (getInput, testPart)

getDay = runIO . getInput 2015

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 1
        testPart 1 D01.solveA input 280
        testPart 2 D01.solveB input (Just 1797)
