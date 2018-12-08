module Queue
    ( Queue

    -- * Construction
    , empty
    , singleton
    , fromList

    -- * Inserting
    , enqueue

    -- * Accessors
    , dequeue
    , length
    , toList
    , null
    ) where

import Prelude hiding (null, length)
import qualified Prelude as P

data Queue a = Queue { _incoming :: [a]
                     , _outgoing :: [a]
                     }

instance (Show a) => Show (Queue a) where
    show q = "fromList " ++ show (toList q)

instance (Eq a) => Eq (Queue a) where
    a == b = toList a == toList b

instance Functor Queue where
    fmap f (Queue i o) = Queue (fmap f i) (fmap f o)

null :: Queue a -> Bool
null (Queue i o) =
    P.null i && P.null o

length :: Queue a -> Int
length (Queue i o) =
    P.length i + P.length o

fromList :: [a] -> Queue a
fromList lst =
    Queue [] lst

empty :: Queue a
empty =
    Queue [] []

singleton :: a -> Queue a
singleton a =
    fromList [a]

toList :: Queue a -> [a]
toList (Queue i o) =
    o ++ reverse i

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue i o) =
    Queue (a:i) o

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] [])    = Nothing
dequeue (Queue i (o:os)) = Just (o, Queue i os)
dequeue (Queue i [])     =
    Just (o, Queue [] os)
    where
        (o:os) = reverse i