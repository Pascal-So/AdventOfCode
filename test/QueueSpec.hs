module QueueSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Queue
import Data.List (foldl')

spec :: Spec
spec = parallel $ do
    describe "toList" $ do
        it "inverts fromList" $ property $
            \list -> toList (fromList list) `shouldBe` (list :: [Int])

        it "works with an empty queue" $ do
            toList (emptyQueue :: Queue Int) `shouldBe` []

    describe "fromList" $ do
        it "returns elements in the correct order" $ property $
            \list ->
                let
                    extractValues q = case deQueue q of
                        Just (val, q') -> val : extractValues q'
                        Nothing -> []
                in
                    extractValues (fromList list) `shouldBe` (list :: [Int])

    describe "deQueue" $ do
        it "works on a previously empty list" $ do
            deQueue (enQueue 1 emptyQueue) `shouldBe` Just (1, emptyQueue)

        it "returns Nothing on an empty list" $ do
            deQueue (emptyQueue :: Queue Int) `shouldBe` Nothing

    describe "enQueue" $ do
        it "results in a correct list" $ property $
            \list -> toList (foldr enQueue emptyQueue $ reverse list) == (list :: [Int])

    describe "empty" $ do
        it "works on an empty list" $
            empty (emptyQueue :: Queue Int)

        it "works on a non-empty list" $
            not $ empty $ fromList [True]

    describe "instance Eq" $ do
        it "works" $ property $
            \list -> deQueue (foldr enQueue emptyQueue (reverse list)) == deQueue (fromList (list :: [Int]))
