module Tape ( Tape
            , moveLeft
            , moveRight
            , head
            , fromList
            , fromLists
            , toList
            , toLists
            , empty
            , isEmpty
            , insert
            , drop
            , adjust
            , update ) where

import Prelude hiding (head, drop)
import qualified Prelude as P
import qualified Data.List as List

data Tape a = Tape 
                { _left  :: [a]
                , _right :: [a]
                }

instance (Show a) => Show (Tape a) where
    show (Tape l r) = "fromLists " ++ show l ++ show r

moveRight :: a -> Tape a -> Tape a
moveRight a (Tape ls [])     = Tape (a:ls) []
moveRight a (Tape ls [r])    = Tape (r:ls) [a]
moveRight a (Tape ls (r:rs)) = Tape (r:ls) rs

moveLeft :: a -> Tape a -> Tape a
moveLeft a (Tape  []    rs) = Tape [] (a:rs)
moveLeft a (Tape  [l]   rs) = Tape [] (l:rs)
moveLeft a (Tape (l:ls) rs) = Tape ls (l:rs)

head :: Tape a -> a
head (Tape _ rs) = P.head rs

fromLists :: [a] -> [a] -> Tape a
fromLists = Tape

toLists :: Tape a -> ([a], [a])
toLists (Tape l r) = (l, r)

toList :: Tape a -> [a]
toList (Tape l r) = reverse l ++ r

fromList :: [a] -> Tape a
fromList = Tape []

isEmpty :: Tape a -> Bool
isEmpty (Tape l r) =
    null l && null r

empty :: Tape a
empty = Tape [] []

insert :: a -> Tape a -> Tape a
insert a (Tape l r) = Tape l (a:r)

drop :: Tape a -> Tape a
drop (Tape l r) = Tape l (tail r)

adjust :: (a -> a) -> Tape a -> Tape a
adjust f (Tape l (r:rs)) = Tape l (f r:rs)

update :: a -> Tape a -> Tape a
update a = adjust (const a)