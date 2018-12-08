module QueueSpec (spec) where

import qualified Prelude as P
import Prelude hiding (length, null)
import Test.Hspec
import Test.QuickCheck
import Queue

spec :: Spec
spec = parallel $ do
    describe "toList" $ do
        it "inverts fromList" $ property $
            \list -> toList (fromList list) `shouldBe` (list :: [Int])

        it "works with an empty queue" $ do
            toList (empty :: Queue Int) `shouldBe` []

    describe "fromList" $ do
        it "returns elements in the correct order" $ property $
            \list ->
                let
                    extractValues q = case dequeue q of
                        Just (val, q') -> val : extractValues q'
                        Nothing -> []
                in
                    extractValues (fromList list) `shouldBe` (list :: [Int])

    describe "dequeue" $ do
        it "works on a previously empty list" $ do
            dequeue (enqueue (1 :: Int) empty) `shouldBe` Just (1, empty)

        it "returns Nothing on an empty list" $ do
            dequeue (empty :: Queue Int) `shouldBe` Nothing

    describe "enqueue" $ do
        it "results in a correct list" $ property $
            \list -> toList (foldr enqueue empty $ reverse list) == (list :: [Int])

    describe "null" $ do
        it "works on an empty list" $
            null (empty :: Queue Int)

        it "works on a non-empty list" $
            not $ null $ fromList [True]

    describe "length" $ do
        it "works" $ property $
            \list -> length (fromList list) == P.length (list :: [Int])

    describe "instance Eq" $ do
        it "works" $ property $
            \list -> dequeue (foldr enqueue empty (reverse list)) == dequeue (fromList (list :: [Int]))

    describe "instance Functor" $ do
        it "works" $ property $
            \list -> toList (f <$> fromList list) == (f <$> list :: [Int])
            where
                f = (+1)
