module UtilsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Utils

import qualified Data.List as List

spec :: Spec
spec = parallel $ do
    describe "sortOn" $ do
        it "works on arbitrary lists" $ property $
            \list ->
                map snd (sortOn snd list) `shouldBe` List.sort (map snd (list :: [(Int, Int)]))

    describe "nubOn" $ do
        it "works on arbitrary lists" $ property $
            \list ->
                map snd (nubOn snd list) `shouldBe` List.nub (map snd (list :: [(Int, Int)]))

    describe "groupOn" $ do
        it "works on arbitrary lists" $ property $
            \list ->
                map (map snd) (groupOn snd list) `shouldBe` List.group (map snd (list :: [(Int, Int)]))
