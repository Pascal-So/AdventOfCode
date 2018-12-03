module Year2017.Day06Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day06 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 6
            solveA input `shouldBe` 7864
        it "solves Part Two" $ do
            input <- getInput 2017 6
            solveB input `shouldBe` 1695
