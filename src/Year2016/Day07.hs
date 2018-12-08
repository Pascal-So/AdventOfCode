{-# LANGUAGE ScopedTypeVariables #-}

module Year2016.Day07 where

import Data.List (sort, group, transpose)
import Utils (sortOn, sortUniq)
import qualified Data.Set as Set

data IP = IP
    { _supernet :: ![String]  -- ^ Supernet sequences (outside the square brackets)
    , _hypernet  :: ![String]  -- ^ Hypernet sequences (inside the square brackets)
    } deriving (Show)

instance Semigroup IP where
    IP sa ha <> IP sb hb =
        IP (sa <> sb) (ha <> hb)

-- * Solvers
solveA :: String -> Int
solveA =
    length . filter supportsTLS . parseInput

solveB :: String -> Int
solveB =
    length . filter supportsSSL . parseInput

-- * Parsing

parseIP :: String -> IP
parseIP [] = IP [] []
parseIP ('[' : content) =
    IP [] [inside] <> parseIP rest
    where
        hypertext = (/= ']')
        inside = takeWhile hypertext content
        rest = tail $ dropWhile hypertext content
parseIP line =
    IP [outside] [] <> parseIP rest
    where
        isoutside = (/= '[')
        outside = takeWhile isoutside line
        rest = dropWhile isoutside line

parseInput :: String -> [IP]
parseInput =
    fmap parseIP . lines

-- * Implementation

-- | Let's not try to be clever here, the bruteforce implementation is perfect for this.
containsABBA :: String -> Bool
containsABBA str = any isABBA candidates
    where
        isABBA [a,b,c,d] = a == d && b == c && a /= b
        candidates = fmap (take 4 . flip drop str) [0 .. length str - 4]

supportsTLS :: IP -> Bool
supportsTLS (IP outer inner) =
    any containsABBA outer && all (not. containsABBA) inner

getABAs :: String -> [String]
getABAs (a:b:c:rest) =
    if a == c && a /= b
        then [a,b,c] : others
        else others
    where others = getABAs (b:c:rest)
getABAs _ = []

flipABA :: String -> String
flipABA (a:b:_) = [b,a,b]

supportsSSL :: IP -> Bool
supportsSSL (IP outer inner) = not $ null $ Set.intersection o i
    where
        o = Set.fromList $ flipABA <$> (outer >>= getABAs)
        i = Set.fromList $ inner >>= getABAs
