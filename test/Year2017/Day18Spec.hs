module Year2017.Day18Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day18 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 18
            solveA input `shouldBe` 3188
        it "solves Part Two" $ do
            input <- getInput 2017 18
            solveB input `shouldBe` 7112
