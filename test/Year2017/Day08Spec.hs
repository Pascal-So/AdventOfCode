module Year2017.Day08Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day08 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 8
            solveA input `shouldBe` 5849
        it "solves Part Two" $ do
            input <- getInput 2017 8
            solveB input `shouldBe` 6702
