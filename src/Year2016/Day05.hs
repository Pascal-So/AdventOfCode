module Year2016.Day05 (solveA, solveB) where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Digest.Pure.MD5
import Data.Maybe (mapMaybe)
import Data.List (sort)
import Utils (nubOn)

pwdInfo :: MD5Digest -> Maybe (Char, Char)
pwdInfo digest =
    if take 5 sdigest == "00000" then
        Just (sdigest !! 5, sdigest !! 6)
    else Nothing
    where sdigest = show digest

getCandidates :: LB.ByteString -> [LB.ByteString]
getCandidates init =
    fmap ((<>) init . LBC.pack . show) [0..]

-- | You spend your whole life trying to stay away from this bitcoin rubbish
-- and then advent of code comes along and makes you mine in haskell on
-- your poor old cpu.
--
-- Anyway this function is really slow so probably don't keep that in the tests.
solveA :: String -> String
solveA input =
    take 8 $ fst <$> mapMaybe (pwdInfo . md5) candidates
    where
        candidates = getCandidates $ LBC.pack input

-- | This is even slower than solveA, it takes like literally 6 minutes so have fun with that.
solveB :: String -> String
solveB input =
    fmap snd $ sort $ take 8 $ nubOn fst goodPairs
    where
        candidates = getCandidates $ LBC.pack input
        allPairs = mapMaybe (pwdInfo . md5) candidates
        goodPairs = filter (\(pos,_) -> pos `elem` ['0' .. '7']) allPairs
