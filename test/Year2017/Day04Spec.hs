module Year2017.Day04Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day04 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 4
            solveA input `shouldBe` 383
        it "solves Part Two" $ do
            input <- getInput 2017 4
            solveB input `shouldBe` 265
