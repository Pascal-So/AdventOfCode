module Array2DSpec (spec) where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Array2D

type Grid = Array2D Int

isSquare :: [[a]] -> Bool
isSquare [] = True
isSquare lsts = all id eqs && not (null $ head lsts)
    where
        lens = length <$> lsts
        eqs = zipWith (==) lens $ tail lens

spec :: Spec
spec = parallel $ do
    let grid = generate (3, 2) 0 :: Grid

    it "returns the value that it was initialized with" $ do
        grid ! (2, 1) `shouldBe` 0
        grid ! (1, 1) `shouldBe` 0

    it "toLists inverts fromLists" $ property $
        \lsts -> isSquare lsts ==> toLists (fromLists lsts) `shouldBe` (lsts :: [[Int]])

    it "can set values" $ do
        (set (1,1) 2 grid) ! (1,1) `shouldBe` 2

    it "leaves other values unchanged on set" $ do
        (set (1,1) 2 grid) ! (0,1) `shouldBe` 0

    it "can update values" $ do
        (update (1,1) (+2) grid) ! (1,1) `shouldBe` 2

    it "leaves other values unchanged on update" $ do
        (update (1,1) (+2) grid) ! (0,1) `shouldBe` 0