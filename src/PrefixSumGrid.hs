module PrefixSumGrid
    ( PrefixSumGrid
    , generate
    , evaluate
    , addRectangle
    )
    where

import Data.Vector.Unboxed (Vector, Unbox, (//), (!))
import qualified Data.Vector.Unboxed as Vec
import Data.List (foldl')

data PrefixSumGrid a = PrefixSumGrid { width :: !Int
                                     , height :: !Int
                                     , vals :: Vector a
                                     } deriving (Show)

generate :: (Num a, Unbox a) => Int -> Int -> PrefixSumGrid a
generate x y = PrefixSumGrid (x + 1) (y + 1) vs
    where vs = Vec.replicate ((x + 1) * (y + 1)) 0

getIndex :: PrefixSumGrid a -> Int -> Int -> Int
getIndex (PrefixSumGrid w h _) x y
    | x >= w || x < 0 = error ("x index (" ++ show x ++ ") out of bounds (" ++ show w ++ ") in getIndex")
    | y >= h = error ("y index (" ++ show y ++ ") out of bounds (" ++ show h ++ ") in getIndex")
    | otherwise = w * y + x

setAt :: (Unbox a) => Int -> a -> Vector a -> Vector a
setAt i val vec = vec // [(i, val)]

addAt :: (Num a, Unbox a) => Int -> a -> Vector a -> Vector a
addAt i diff vec =
    setAt i ((vec!i) + diff) vec

addRectangle :: (Num a, Unbox a) => (Int, Int) -> (Int, Int) -> a -> PrefixSumGrid a -> PrefixSumGrid a
addRectangle (sx, sy) (lx, ly) val ps@(PrefixSumGrid w h vs) =
    PrefixSumGrid w h resVs
    where
        addGrid x y = if x < w && y < h then addAt (getIndex ps x y) else (\x y -> y)
        resVs = addGrid (sx + 1) (sy + 1) val . addGrid (sx + lx + 1) (sy + 1) (-val) . addGrid (sx + 1) (sy + ly + 1) (-val) . addGrid (sx + lx + 1) (sy + ly + 1) val $ vs

group :: Int -> [a] -> [[a]]
group _ [] = []
group n lst
  | n > 0 = (take n lst) : (group n (drop n lst))
  | otherwise = error "Negative n"

evaluate :: (Num a, Unbox a) => PrefixSumGrid a -> [[a]]
evaluate ps@(PrefixSumGrid w h vs) =
    map tail . tail . group w . Vec.toList $ foldl' unrollStep vs [(x, y) | x <- [1 .. w - 1], y <- [1 .. h - 1]]
    where
        getIdx = getIndex ps
        unrollStep vs (x,y) =
            add (vs ! getIdx (x - 1) (y)) . add (vs ! getIdx (x) (y - 1)) . add (0 - (vs ! getIdx (x - 1) (y - 1))) $ vs
            where
                add = addAt (getIdx x y)
