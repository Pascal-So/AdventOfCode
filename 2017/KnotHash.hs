module KnotHash (sparseHash, hash) where
import Data.Bits (xor)
import Data.List
import Data.Char (ord)
import Numeric (showHex)

hash :: String -> String
hash lst = 
    denseHash . sparseHash $ repeatN 64 (map ord lst ++ suffix)
    where 
        suffix = [17, 31, 73, 47, 23]

sparseHash :: [Int] -> [Int]
sparseHash lst =
    let
        initial = [0..255]
        f (lst, pos) (skip, len) =
            (reverseSegment pos len lst, (pos + len + skip) `mod` (length lst))
    in
        fst $ foldl f (initial, 0) $ zip [0..] lst

denseHash :: [Int] -> String
denseHash =
   concat . map toHex . map (foldl xor 0) . groupsOf 16
   where
        toHex n = leftPad 2 '0' $ showHex n ""

leftPad :: Int -> a -> [a] -> [a]
leftPad len c str =
    replicate (len - length str) c ++ str

repeatN :: Int -> [a] -> [a]
repeatN n = concat . replicate n

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n lst =
    if length lst <= n then
        [lst]
    else
        (take n lst) : (groupsOf n (drop n lst))

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n lst = take (length lst) $ drop n $ cycle lst

reverseSegment :: Int -> Int -> [a] -> [a]
reverseSegment start len lst =
    let
        rlist = rotate start lst
        reversed = reverse $ take len rlist
    in
        rotate (length lst - start) $ reversed ++ drop len rlist
