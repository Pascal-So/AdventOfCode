module Queue (Queue, enQueue, deQueue, fromList, empty, toList, emptyQueue) where

data Queue a = Queue { _incoming :: [a]
                     , _outgoing :: [a]
                     }

instance (Show a) => Show (Queue a) where
    show q = "fromList " ++ show (toList q)

instance (Eq a) => Eq (Queue a) where
    a == b = toList a == toList b

empty :: Queue a -> Bool
empty (Queue i o) =
    null i && null o

fromList :: [a] -> Queue a
fromList lst =
    Queue [] lst

emptyQueue :: Queue a
emptyQueue =
    Queue [] []

toList :: Queue a -> [a]
toList (Queue i o) =
    o ++ reverse i

enQueue :: a -> Queue a -> Queue a
enQueue a (Queue i o) =
    Queue (a:i) o

deQueue :: Queue a -> Maybe (a, Queue a)
deQueue (Queue [] [])    = Nothing
deQueue (Queue i (o:os)) = Just (o, Queue i os)
deQueue (Queue i [])     =
    Just (o, Queue [] os)
    where
        (o:os) = reverse i