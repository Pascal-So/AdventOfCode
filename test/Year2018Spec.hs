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
import qualified Year2018.Day11 as D11
import qualified Year2018.Day12 as D12
import qualified Year2018.Day13 as D13

import Utils (getInput, getExample, test, slow)

getDay = runIO . getInput 2018
getEx day example = runIO $ getExample 2018 day example

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 1
        test "part 1" D01.solveA input 439
        test "part 2" D01.solveB input 124645

    describe "Day 02" $ do
        input <- getDay 2
        test "part 1" D02.solveA input 5727
        test "part 2" D02.solveB input "uwfmdjxyxlbgnrotcfpvswaqh"

    describe "Day 03" $ do
        input <- getDay 3
        test "part 1" D03.solveA input 104126
        test "part 2" D03.solveB input 695

    describe "Day 04" $ do
        input <- getDay 4
        test "part 1" D04.solveA input 21083
        test "part 2" D04.solveB input 53024

    describe "Day 05" $ do
        input <- getDay 5
        test "part 1" D05.solveA input 11310
        test "part 2" D05.solveB input 6020

    describe "Day 06" $ do
        input <- getDay 6
        slow $ test "part 1" D06.solveA input 4186
        test "part 2" D06.solveB input 45509

    describe "Day 07" $ do
        input <- getDay 7
        test "part 1" D07.solveA input "DFOQPTELAYRVUMXHKWSGZBCJIN"
        test "part 2" D07.solveB input 1036

    describe "Day 08" $ do
        input <- getDay 8
        test "part 1" D08.solveA input 37439
        test "part 2" D08.solveB input 20815

    describe "Day 09" $ do
        input <- getDay 9
        test "part 1" D09.solveA input 418237
        -- takes about 15 seconds
        slow $ test "part 2" D09.solveB input 3505711612

    -- Day 10 requires manual work to evaluate and can't
    -- be tested automatically.
    -- For my input: time 10519 => PLBPGFRR

    describe "Day 11" $ do
        input <- getDay 11
        ex1   <- getEx  11 01
        ex2   <- getEx  11 02
        test "part 1 example 1" D11.solveA ex1   "33,45"
        test "part 1 example 2" D11.solveA ex2   "21,61"
        test "part 1"           D11.solveA input "241,40"
        slow $ test "part 2 example 1" D11.solveB ex1   "90,269,16"
        slow $ test "part 2 example 2" D11.solveB ex2   "232,251,12"
        slow $ test "part 2"           D11.solveB input "166,75,12"

    describe "Day 12" $ do
        input <- getDay 12
        ex1   <- getEx  12 01
        test "part 1 example 1" D12.solveA ex1   325
        test "part 1"           D12.solveA input 3903
        test "part 2"           D12.solveB input 3450000002268

    describe "Day 13" $ do
        input <- getDay 13
        ex1   <- getEx  13 01
        test "part 1 example 1" D13.solveA ex1   "7,3"
        test "part 1"           D13.solveA input ""
