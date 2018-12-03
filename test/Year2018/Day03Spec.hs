module Year2018.Day03Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day03 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2018 3
            solveA input `shouldBe` 104126
        it "solves Part Two" $ do
            input <- getInput 2018 3
            solveB input `shouldBe` 695
