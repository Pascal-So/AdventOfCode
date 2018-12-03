module Year2018.Day01Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day01 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2018 1
            solveA input `shouldBe` 439
        it "solves Part Two" $ do
            input <- getInput 2018 1
            solveB input `shouldBe` 124645
