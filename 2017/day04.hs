import Data.List
import Control.Applicative

validPhrase :: (Eq a, Ord a) => [a] -> Bool
validPhrase xs = 
    distinct $ sort xs
    where
        distinct (a:b:xs) =
            if a == b then
                False
            else
                distinct (b:xs)
        distinct _ = True

solveA :: [[String]] -> Int
solveA =
    length . filter validPhrase

solveB :: [[String]] -> Int
solveB = 
    length . filter (validPhrase . map sort)

main :: IO ()
main = do
    input <- map words . lines <$> getContents
    print $ solveA input