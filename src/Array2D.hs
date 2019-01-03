{-|

A simple 2d Array with constant time access and updates

-}

module Array2D
    ( Array2D
    , generate
    , set
    , update
    , fromLists
    , toLists
    , (!)
    )
    where

import Data.Vector (Vector, (//))
import qualified Data.Vector as Vec
import Data.List (foldl')
import Utils (groupOn)

data Array2D a = Array2D { width  :: !Int
                         , height :: !Int
                         , vals   :: Vector a
                         }

instance Functor Array2D where
    fmap f (Array2D w h vals) = Array2D w h (fmap f vals)

-- | Fill an array (width, height) with a default value
generate :: (Int, Int) -> a -> Array2D a
generate (w, h) val =
    Array2D w h $ Vec.replicate (w * h) val

getIndex :: Array2D a -> (Int, Int) -> Int
getIndex (Array2D w h _) (x, y)
    | x >= w || x < 0 = error ("x index (" ++ show x ++ ") out of bounds (" ++ show w ++ ")")
    | y >= h || y < 0 = error ("y index (" ++ show y ++ ") out of bounds (" ++ show h ++ ")")
    | otherwise = h * x + y

setAt :: Int -> a -> Vector a -> Vector a
setAt i val vec = vec // [(i, val)]

-- | Change the value at one coordinate
set :: (Int, Int) -> a -> Array2D a -> Array2D a
set pos val arr@(Array2D w h vals) =
    Array2D w h $ setAt (getIndex arr pos) val vals

-- | Change the value at one coordinate
update :: (Int, Int) -> (a -> a) -> Array2D a -> Array2D a
update pos f arr@(Array2D w h vals) =
    Array2D w h $ setAt idx (f $ vals Vec.! idx) vals
    where
        idx = getIndex arr pos

-- | Access element
(!) :: Array2D a -> (Int, Int) -> a
arr@(Array2D _ _ vals) ! pos =
    vals Vec.! getIndex arr pos

toLists :: Array2D a -> [[a]]
toLists (Array2D w h vals) =
    fmap (fmap fst) $ groupOn snd $ zip (Vec.toList vals) [i `div` h | i <- [0..]]

-- | Undefined behaviour on jagged arrays
fromLists :: [[a]] -> Array2D a
fromLists lsts =
    if w * h == Vec.length vals
        then Array2D w h vals
        else error "Input 2d list to Array2D.fromLists is not square"
    where
        vals = Vec.fromList $ concat lsts
        w = length lsts
        h = if null lsts
            then 0
            else length $ head lsts
