module Year2017.Day03Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day03 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 3
            solveA input `shouldBe` 371
        it "solves Part Two" $ do
            input <- getInput 2017 3
            solveB input `shouldBe` 369601
