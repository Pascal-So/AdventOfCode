module Year2017.Day17 (solveA, solveB) where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Foldable

type Buffer = (Seq Int, Int)

step :: Int -> Buffer -> Buffer
step n (xs, pos) =
    (xs, (pos + n) `mod` len)
    where
        len = max (length xs) 1

insert :: Int -> Buffer -> Buffer
insert n (xs, pos) =
    (Seq.insertAt (pos+1) n xs, pos+1)

finalBuffer :: Int -> Buffer
finalBuffer n =
    foldl' (\buf x ->  insert x $ step n buf) (Seq.empty,0) [0..2017]

solveA :: String -> Int
solveA input =
    Seq.index xs $ (pos + 1) `mod` (length xs)
    where
        n = read input
        (xs, pos) = finalBuffer n

solveB :: String -> Int
solveB input =
    fst $ foldl' go (1,1) [2 .. 50*1000*1000]
    where
        mov = read input
        go (acc,pos) len =
            if newpos == 0 then
                (len, 1)
            else
                (acc, newpos + 1)
            where
                newpos = (pos + mov) `mod` len
