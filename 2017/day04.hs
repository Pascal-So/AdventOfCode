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


main :: IO ()
main = do
    phrases <- lines <$> getContents
    print $ length $ filter (validPhrase . map sort . words) phrases