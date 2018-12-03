module Year2017Spec (spec) where

import Test.Hspec (Spec, describe, runIO, parallel)
import qualified Year2017.Day01 as D01
import qualified Year2017.Day02 as D02
import qualified Year2017.Day03 as D03
import qualified Year2017.Day04 as D04
import qualified Year2017.Day05 as D05
import qualified Year2017.Day06 as D06
import qualified Year2017.Day07 as D07
import qualified Year2017.Day08 as D08
import qualified Year2017.Day09 as D09
import qualified Year2017.Day10 as D10
import qualified Year2017.Day11 as D11
import qualified Year2017.Day12 as D12
import qualified Year2017.Day13 as D13
import qualified Year2017.Day14 as D14
import qualified Year2017.Day15 as D15
import qualified Year2017.Day16 as D16
import qualified Year2017.Day17 as D17
import qualified Year2017.Day18 as D18
import qualified Year2017.Day19 as D19
import qualified Year2017.Day20 as D20
import qualified Year2017.Day21 as D21
import qualified Year2017.Day22 as D22
import qualified Year2017.Day23 as D23
import qualified Year2017.Day24 as D24
import qualified Year2017.Day25 as D25
import Utils (getInput, testPart)

getDay = runIO . getInput 2017

spec :: Spec
spec = parallel $ do
    describe "Day 01" $ do
        input <- getDay 01
        testPart 1 D01.solveA input $ 1251
        testPart 2 D01.solveB input $ 1244
    describe "Day 02" $ do
        input <- getDay 02
        testPart 1 D02.solveA input $ 43074
        testPart 2 D02.solveB input $ 280
    describe "Day 03" $ do
        input <- getDay 03
        testPart 1 D03.solveA input $ 371
        testPart 2 D03.solveB input $ 369601
    describe "Day 04" $ do
        input <- getDay 04
        testPart 1 D04.solveA input $ 383
        testPart 2 D04.solveB input $ 265
    describe "Day 05" $ do
        input <- getDay 05
        testPart 1 D05.solveA input $ 355965
        testPart 2 D05.solveB input $ 26948068
    describe "Day 06" $ do
        input <- getDay 06
        testPart 1 D06.solveA input $ 7864
        testPart 2 D06.solveB input $ 1695
    describe "Day 07" $ do
        input <- getDay 07
        testPart 1 D07.solveA input $ "veboyvy"
        testPart 2 D07.solveB input $ Just 749
    describe "Day 08" $ do
        input <- getDay 08
        testPart 1 D08.solveA input $ 5849
        testPart 2 D08.solveB input $ 6702
    describe "Day 09" $ do
        input <- getDay 09
        testPart 1 D09.solveA input $ 12396
        testPart 2 D09.solveB input $ 6346
    describe "Day 10" $ do
        input <- getDay 10
        testPart 1 D10.solveA input $ 1935
        testPart 2 D10.solveB input $ "dc7e7dee710d4c7201ce42713e6b8359"
    describe "Day 11" $ do
        input <- getDay 11
        testPart 1 D11.solveA input $ 687
        testPart 2 D11.solveB input $ 1483
    describe "Day 12" $ do
        input <- getDay 12
        testPart 1 D12.solveA input $ 239
        testPart 2 D12.solveB input $ 215
    describe "Day 13" $ do
        input <- getDay 13
        testPart 1 D13.solveA input $ 1840
        testPart 2 D13.solveB input $ 3850260
    describe "Day 14" $ do
        input <- getDay 14
        testPart 1 D14.solveA input $ 8190
        testPart 2 D14.solveB input $ 1134
    describe "Day 15" $ do
        input <- getDay 15
        testPart 1 D15.solveA input $ 631
        testPart 2 D15.solveB input $ 279
    describe "Day 16" $ do
        input <- getDay 16
        testPart 1 D16.solveA input $ "giadhmkpcnbfjelo"
        testPart 2 D16.solveB input $ "njfgilbkcoemhpad"
    describe "Day 17" $ do
        input <- getDay 17
        testPart 1 D17.solveA input $ 1561
        testPart 2 D17.solveB input $ 33454823
    describe "Day 18" $ do
        input <- getDay 18
        testPart 1 D18.solveA input $ 3188
        testPart 2 D18.solveB input $ 7112
    describe "Day 19" $ do
        input <- getDay 19
        testPart 1 D19.solveA input $ "SXWAIBUZY"
        testPart 2 D19.solveB input $ 16676
    describe "Day 20" $ do
        input <- getDay 20
        testPart 1 D20.solveA input $ 150
        testPart 2 D20.solveB input $ 657
    describe "Day 21" $ do
        input <- getDay 21
        testPart 1 D21.solveA input $ 176
        testPart 2 D21.solveB input $ 2368161
    describe "Day 22" $ do
        input <- getDay 22
        testPart 1 D22.solveA input $ 5330
        testPart 2 D22.solveB input $ 2512103
    describe "Day 23" $ do
        input <- getDay 23
        testPart 1 D23.solveA input $ 8281
        testPart 2 D23.solveB ()    $ 911
    describe "Day 24" $ do
        input <- getDay 24
        testPart 1 D24.solveA input $ 2006
        testPart 2 D24.solveB input $ 1994
    describe "Day 25" $ do
        -- input is hardcoded for day 25
        testPart 1 D25.solveA ()    $ 2526
