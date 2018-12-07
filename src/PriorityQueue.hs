module PriorityQueue
    ( PriorityQueue
    , empty
    , singleton
    , union
    , insert
    , add
    , fromList
    , toList
    , map
    , null
    , minView
    ) where

-- A simple wrapper around Data.PriorityQueue.FingerTree to make
-- priority queues where the values are the keys themselves.

import Prelude hiding (null, map)
import Data.List (unfoldr)
import qualified Data.PriorityQueue.FingerTree as PQ

newtype PriorityQueue a = PQ (PQ.PQueue a a) deriving (Eq, Show)

map :: (Ord a, Ord b) => (a -> b) -> PriorityQueue a -> PriorityQueue b
map f = fromList . fmap f . toList

empty :: (Ord a) => PriorityQueue a
empty = PQ PQ.empty

singleton :: (Ord a) => a -> PriorityQueue a
singleton v = PQ (PQ.singleton v v)

union :: (Ord a) => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
union (PQ a) (PQ b) = PQ (PQ.union a b)

insert :: (Ord a) => a -> PriorityQueue a -> PriorityQueue a
insert v q = union (singleton v) q

add :: (Ord a) => a -> PriorityQueue a -> PriorityQueue a
add v q = union q (singleton v)

fromList :: (Ord a) => [a] -> PriorityQueue a
fromList as = PQ (PQ.fromList $ zip as as)

toList :: (Ord a) => PriorityQueue a -> [a]
toList = unfoldr minView

null :: (Ord a) => PriorityQueue a -> Bool
null (PQ q) = PQ.null q

minView :: (Ord a) => PriorityQueue a -> Maybe (a, PriorityQueue a)
minView (PQ q) = do
    (val, rest) <- PQ.minView q
    return (val, PQ rest)
