module Year2018Spec (spec) where

import Test.Hspec (Spec, describe, runIO, parallel)
import qualified Year2018.Day01 as D01
import qualified Year2018.Day02 as D02
import qualified Year2018.Day03 as D03
import qualified Year2018.Day04 as D04
import qualified Year2018.Day05 as D05
import qualified Year2018.Day06 as D06
import qualified Year2018.Day07 as D07
import qualified Year2018.Day08 as D08
import qualified Year2018.Day09 as D09
import Utils (getInput, testPart)

getDay = runIO . getInput 2018

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 1
        testPart 1 D01.solveA input 439
        testPart 2 D01.solveB input 124645

    describe "Day 02" $ do
        input <- getDay 2
        testPart 1 D02.solveA input 5727
        testPart 2 D02.solveB input "uwfmdjxyxlbgnrotcfpvswaqh"

    describe "Day 03" $ do
        input <- getDay 3
        testPart 1 D03.solveA input 104126
        testPart 2 D03.solveB input 695

    describe "Day 04" $ do
        input <- getDay 4
        testPart 1 D04.solveA input 21083
        testPart 2 D04.solveB input 53024

    describe "Day 05" $ do
        input <- getDay 5
        testPart 1 D05.solveA input 11310
        testPart 2 D05.solveB input 6020

    describe "Day 06" $ do
        input <- getDay 6
        -- testPart 1 D06.solveA input 4186
        testPart 2 D06.solveB input 45509

    describe "Day 07" $ do
        input <- getDay 7
        testPart 1 D07.solveA input "DFOQPTELAYRVUMXHKWSGZBCJIN"
        testPart 2 D07.solveB input 1036

    describe "Day 08" $ do
        input <- getDay 8
        testPart 1 D08.solveA input 37439
        testPart 2 D08.solveB input 20815

    describe "Day 09" $ do
        input <- getDay 9
        testPart 1 D09.solveA input 418237
        -- takes about 15 seconds
        -- testPart 2 D09.solveB input 3505711612

    -- Day 10 requires manual work to evaluate and can't
    -- be tested automatically.
