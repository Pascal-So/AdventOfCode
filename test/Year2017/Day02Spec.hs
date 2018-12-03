module Year2017.Day02Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day02 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 2
            solveA input `shouldBe` 43074
        it "solves Part Two" $ do
            input <- getInput 2017 2
            solveB input `shouldBe` 280
