module Year2018.Day02Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2018.Day02 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2018 2
            solveA input `shouldBe` 5727
        it "solves Part Two" $ do
            input <- getInput 2018 2
            solveB input `shouldBe` "uwfmdjxyxlbgnrotcfpvswaqh"
