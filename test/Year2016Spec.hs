module Year2016Spec (spec) where

import Test.Hspec (Spec, describe, runIO, parallel)
import qualified Year2016.Day01 as D01
import qualified Year2016.Day02 as D02
import qualified Year2016.Day03 as D03
import qualified Year2016.Day04 as D04
import qualified Year2016.Day05 as D05
import qualified Year2016.Day06 as D06
import qualified Year2016.Day07 as D07

import Utils (getInput, test, slow)

getDay = runIO . getInput 2016

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 1
        test "part 1" D01.solveA input 234
        test "part 2" D01.solveB input (Just 113)

    describe "Day 02" $ do
        input <- getDay 2
        test "part 1" D02.solveA input "95549"
        test "part 2" D02.solveB input "D87AD"

    describe "Day 03" $ do
        input <- getDay 3
        test "part 1" D03.solveA input 917
        test "part 2" D03.solveB input 1649

    describe "Day 04" $ do
        input <- getDay 4
        test "part 1" D04.solveA input 158835
        test "part 2" D04.solveB input 993

    -- These tests take about 8 minutes together..
    describe "Day 05" $ do
       input <- getDay 5
       slow $ test "part 1" D05.solveA input "f77a0e6e"
       slow $ test "part 2" D05.solveB input "999828ec"

    describe "Day 06" $ do
        input <- getDay 6
        test "part 1" D06.solveA input "qtbjqiuq"
        test "part 2" D06.solveB input "akothqli"

    describe "Day 07" $ do
        input <- getDay 7
        test "part 1" D07.solveA input 110
        test "part 2" D07.solveB input 242
