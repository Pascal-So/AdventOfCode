module Year2017.Day10Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Year2017.Day10 (solveA, solveB)
import Utils (getInput)

spec :: Spec
spec =
    describe "solutions" $ do
        it "solves Part One" $ do
            input <- getInput 2017 10
            solveA input `shouldBe` 1935
        it "solves Part Two" $ do
            input <- getInput 2017 10
            solveB input `shouldBe` "dc7e7dee710d4c7201ce42713e6b8359"
