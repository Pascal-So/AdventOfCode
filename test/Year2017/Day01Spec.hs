module Year2017.Day01Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day01 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 1
            solveA input `shouldBe` 1251
        it "solves Part Two" $ do
            input <- getInput 2017 1
            solveB input `shouldBe` 1244
