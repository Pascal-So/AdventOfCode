module Year2018Spec (spec) where

import Test.Hspec (Spec, describe, runIO, parallel)
import qualified Year2018.Day01 as D01
import qualified Year2018.Day02 as D02
import qualified Year2018.Day03 as D03
import qualified Year2018.Day04 as D04
import Utils (getInput, testPart)

getDay = runIO . getInput 2018

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 1
        testPart 1 D01.solveA input $ 439
        testPart 2 D01.solveB input $ 124645

    describe "Day 02" $ do
        input <- getDay 2
        testPart 1 D02.solveA input $ 5727
        testPart 2 D02.solveB input $ "uwfmdjxyxlbgnrotcfpvswaqh"

    describe "Day 03" $ do
        input <- getDay 3
        -- testPart 1 D03.solveA input $ 104126
        testPart 2 D03.solveB input $ 695

    describe "Day 04" $ do
        input <- getDay 4
        testPart 1 D04.solveA input $ 21083
        testPart 2 D04.solveB input $ 53024
