module Year2017.Day07Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day07 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 7
            solveA input `shouldBe` "veboyvy"
        it "solves Part Two" $ do
            input <- getInput 2017 7
            solveB input `shouldBe` Just 749
