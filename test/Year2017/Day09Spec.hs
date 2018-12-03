module Year2017.Day09Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day09 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 9
            solveA input `shouldBe` 12396
        it "solves Part Two" $ do
            input <- getInput 2017 9
            solveB input `shouldBe` 6346
