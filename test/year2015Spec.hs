module Year2015Spec (spec) where

import Test.Hspec (Spec, describe, runIO, parallel)
import qualified Year2015.Day01 as D01
import Utils (getInput, test)

getDay = runIO . getInput 2015

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 1
        test "part 1" D01.solveA input 280
        test "part 2" D01.solveB input (Just 1797)
