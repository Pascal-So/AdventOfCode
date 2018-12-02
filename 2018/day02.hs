import Data.Vector (Vector, (//), (!))
import Data.List
import Data.Maybe

checksum :: [String] -> Int
checksum input =
    twos * threes
    where
        occurrence_numbers = map (map length . group . sort) input
        twos = length $ filter (elem 2) occurrence_numbers
        threes = length $ filter (elem 3) occurrence_numbers

solveA :: IO ()
solveA = do
    input <- lines <$> getContents
    print $ checksum input

removeOne :: Int -> [a] -> [a]
removeOne 0 (_:rest) = rest
removeOne n (x:rest) = x : removeOne (n - 1) rest

findDuplicate :: (Eq a, Ord a) => [a] -> Maybe a
findDuplicate lst =
    fmap fst $ find (\(a, b) -> a == b) $ zip sorted $ tail sorted
    where
        sorted = sort lst

solveB :: IO ()
solveB = do
    input <- lines <$> getContents
    print . head . catMaybes $ map (\n -> findDuplicate $ map (removeOne n) input) [0 .. length (head input)]

main = solveB
