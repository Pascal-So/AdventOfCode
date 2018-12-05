module Year2018.Day05 (solveA, solveB) where

import Data.Char
import Data.List

type Unit = (Char, Bool)

reducible :: Unit -> Unit -> Bool
reducible (a,pola) (b,polb) =
    a == b && pola /= polb

readInput :: String -> [Unit]
readInput = map parseUnit
    where
        parseUnit a = (toLower a, isUpper a)

getTypes :: [Unit] -> [Char]
getTypes = map head . group . sort . map fst

reduce :: [Unit] -> [Unit]
reduce = go []
    where
        go [] (x:xs) = go [x] xs
        go lst [] = lst
        go (x:xs) (y:ys) =
            if reducible x y then
                go xs ys
            else
                go (y:x:xs) ys

removeType :: Char -> [Unit] -> [Unit]
removeType c = filter (\(x,_) -> x /= c)

solveA :: String -> Int
solveA = length . reduce . readInput

solveB :: String -> Int
solveB input = minimum $ map length reduceds
    where
        semiReduced = reduce $ readInput input
        reduceds = [reduce $ removeType c semiReduced | c <- getTypes semiReduced]
