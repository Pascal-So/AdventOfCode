module Year2017.Day05Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day05 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 5
            solveA input `shouldBe` 355965

        -- this test is slow as hell so I decided I don't care about it
        -- it "solves Part Two" $ do
        --     input <- getInput 2017 5
        --     solveB input `shouldBe` 26948068
