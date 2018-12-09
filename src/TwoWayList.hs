module TwoWayList
    ( TWL
    , toList
    , toLists
    , fromList
    , fromLists
    , null
    , (!)
    , index
    , update
    , adjust
    , shift
    , length
    , insertLeft
    , insertRight
    , removeLeft
    , removeRight
    ) where

import Prelude hiding (null, length)
import qualified Prelude as P

data TWL a = TWL { _left  :: [a]
                 , _right :: [a]
                 }

instance (Show a) => Show (TWL a) where
    show (TWL l r) = "fromLists " ++ show l ++ " " ++ show r

instance Functor TWL where
    fmap f (TWL l r) = TWL (fmap f l) (fmap f r)

toList :: TWL a -> [a]
toList (TWL l r) = (reverse l) ++ r

toLists :: TWL a -> ([a],[a])
toLists (TWL l r) = (l, r)

fromList :: [a] -> TWL a
fromList lst = TWL [] lst

fromLists :: [a] -> [a] -> TWL a
fromLists = TWL

null :: TWL a -> Bool
null (TWL l r) =
    P.null l && P.null r

length :: TWL a -> Int
length (TWL l r) =
    P.length l + P.length r

index :: TWL a -> Int -> a
index (TWL l r) n =
    if n >= 0 then
        r !! n
    else
        l !! (-n - 1)

(!) :: TWL a -> Int -> a
(!) = index

update :: Int -> a -> TWL a -> TWL a
update n val =
    adjust (const val) n

adjust :: (a -> a) -> Int -> TWL a -> TWL a
adjust f n (TWL l r) =
    if n >= 0 then
        let
            adjusted = take n r ++ [f (r !! n)] ++ drop (n+1) r
        in
            TWL l adjusted
    else
        let
            m = -n - 1
            adjusted = take m l ++ [f (l !! m)] ++ drop (m+1) l
        in
            TWL adjusted r

shift :: Int -> TWL a -> TWL a
shift n (TWL l r) =
    if n >= 0 then
        TWL (reverse (take n r) ++ l) (drop n r)
    else
        TWL (drop (-n) l) (reverse (take (-n) l) ++ r)

insertLeft :: a -> TWL a -> TWL a
insertLeft a (TWL l r) = TWL (a:l) r

insertRight :: a -> TWL a -> TWL a
insertRight a (TWL l r) = TWL l (a:r)

removeLeft :: TWL a -> TWL a
removeLeft (TWL (l:ls) r) = TWL ls r

removeRight :: TWL a -> TWL a
removeRight (TWL l (r:rs)) = TWL l rs
