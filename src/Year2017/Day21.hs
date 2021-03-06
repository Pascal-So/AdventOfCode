module Year2017.Day21 (solveA, solveB) where

import Prelude hiding (flip)
import qualified Data.List as List
import Data.Function (on)

type Square = [[Bool]]

type Rule = (Square, Square)

rotate :: Square -> Square
rotate = List.transpose . flip

flip :: Square -> Square
flip = reverse

match :: Square -> Square -> Bool
match a b =
    a == b ||
    a == rotate b ||
    a == rotate (rotate b) ||
    a == rotate (rotate (rotate b)) ||
    a == flip b ||
    a == rotate (flip b) ||
    a == rotate (rotate (flip b)) ||
    a == rotate (rotate (rotate (flip b)))

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs =
    take n xs : groupsOf n (drop n xs)

breakSquares :: Square -> [[Square]]
breakSquares sq =
    map (List.transpose . map (groupsOf n)) $ groupsOf n sq
    where
        len = length sq
        n = if len `mod` 2 == 0 then 2 else 3

combineSquares :: [[Square]] -> Square
combineSquares sqs =
    List.concat $ map (map List.concat . List.transpose) sqs

applyRule :: [Rule] -> Square -> Square
applyRule rules sq =
    snd $ head $ filter (match sq . fst) rules


step :: [Rule] -> Square -> Square
step rules =
    combineSquares . map (map (applyRule rules)) . breakSquares

initSquare :: Square
initSquare =
    [ [False, True,  False]
    , [False, False, True ]
    , [True,  True,  True ]
    ]

lineToSquare :: String -> Square
lineToSquare str =
    map (map ((== '#') . snd)) . List.groupBy ((==)`on`fst) $ numbered
    where
        filtered = filter (/= '/') str
        len = floor $ sqrt $ (fromIntegral (length filtered) :: Double)
        numbered = zip ([0..] >>= replicate len) filtered

readRule :: String -> Rule
readRule str =
    (lineToSquare (w!!0), lineToSquare (w!!2))
    where
        w = words str

readInput :: String -> [Rule]
readInput =
    map readRule . lines

solveA :: String -> Int
solveA input =
    sum $ map (length . filter id) final
    where
        rules = readInput input
        final = (iterate (step rules) initSquare) !! 5

solveB :: String -> Int
solveB input =
    sum $ map (length . filter id) final
    where
        rules = readInput input
        final = (iterate (step rules) initSquare) !! 18
