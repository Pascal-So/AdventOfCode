module PrefixSumGridSpec (spec) where

import Test.Hspec
import PrefixSumGrid
import Debug.Trace

type Grid = PrefixSumGrid Int

spec :: Spec
spec = parallel $ do
    let grid = generate 3 2 :: Grid

    it "returns zero on empty" $ do
        evaluate grid `shouldBe` [[0,0,0],[0,0,0]]

    it "works with a 0x0 rectangle" $ do
        let withRect = addRectangle (0,0) (0,0) 1 grid
        evaluate withRect `shouldBe` [[0,0,0],[0,0,0]]

    it "works with a 1x1 rectangle" $ do
        let withRect = addRectangle (0,0) (1,1) 42 grid
        evaluate withRect `shouldBe` [[42,0,0],[0,0,0]]

    it "works with a shifted 2x1 rectangle" $ do
        let withRect = addRectangle (1,0) (2,1) 3 grid
        evaluate withRect `shouldBe` [[0,3,3],[0,0,0]]

    it "works with a full rectangle" $ do
        let withRect = addRectangle (0,0) (3,2) 1 grid
        evaluate withRect `shouldBe` [[1,1,1],[1,1,1]]

    it "works with overlapping rectangle" $ do
        let withRect = addRectangle (1,1) (2,1) 2 $ addRectangle (0,0) (2,2) 1 grid
        evaluate withRect `shouldBe` [[1,1,0],[1,3,2]]

    it "works with cancelling rectangles" $ do
        let withRect = addRectangle (0,0) (2,2) (-1) $ addRectangle (0,0) (2,2) 1 grid
        evaluate withRect `shouldBe` [[0,0,0],[0,0,0]]
