module Year2017.Day16 (solveA, solveB) where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Foldable
import Data.Char

data Instruction = Spin Int | Exchange Int Int | Partner Char Char

spin :: Int -> Seq a -> Seq a
spin n xs =
    back >< front
    where
        (front, back) = Seq.splitAt (length xs - n) xs

exchange :: Int -> Int -> Seq a -> Seq a
exchange a b xs =
    Seq.update a vb $ Seq.update b va xs
    where
        va = Seq.index xs a
        vb = Seq.index xs b

partner :: (Eq a) => a -> a -> Seq a -> Seq a
partner a b xs =
    exchange pa pb xs
    where
        pa = 0 `fromMaybe` Seq.elemIndexL a xs
        pb = 0 `fromMaybe` Seq.elemIndexL b xs

runInstruction :: Instruction -> Seq Char -> Seq Char
runInstruction (Spin n) = spin n
runInstruction (Exchange a b) = exchange a b
runInstruction (Partner a b) = partner a b

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c str =
    block : bs
    where
        (block, rest) = break (== c) str
        bs = if null rest
                then []
                else splitBy c (tail rest)

readInstruction :: String -> Instruction
readInstruction ('s':xs) = Spin (read xs)
readInstruction ('x':xs) =
    Exchange (read a) (read b)
        where
            [a,b] = splitBy '/' xs
readInstruction ('p':xs) =
    Partner (head a) (head b)
        where
            [a,b] = splitBy '/' xs

readInput :: String -> [Instruction]
readInput =
    map readInstruction . splitBy ','

startArrangement :: Seq Char
startArrangement =
    Seq.fromList ['a' .. 'p']

showArrangement :: Seq Char -> String
showArrangement = toList

solveDance :: Seq Char -> [Instruction] -> Seq Char
solveDance = foldl (flip runInstruction)

solveA :: String -> String
solveA =
    showArrangement . solveDance startArrangement . readInput

solveB :: String -> String
solveB input =
    showArrangement result
    where
        insts = readInput input
        result = (iterate (flip solveDance insts) startArrangement) !! (1000000000 `mod` 36)
