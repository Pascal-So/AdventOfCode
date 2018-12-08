{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day08 where

data Node = Node [Node] [Int]

-- * Solvers

-- | Was.. was this task written specifically with haskell in mind?
solveA :: String -> Int
solveA input = sumHeaders root
    where
        root = readInput input

solveB :: String -> Int
solveB input = value root
    where
        root = readInput input

-- * Parsing

parseNode :: [Int] -> (Node, [Int])
parseNode (0:m:content) = (Node [] (take m content), drop m content)
parseNode (c:m:content) = (Node children headers, rest)
    where
        scanned = take c $ scanUnfoldr parseNode content
        children = fst <$> scanned
        remcontent = snd $ last scanned
        headers = take m remcontent
        rest = drop m remcontent

-- | Throws a pattern match error if the input is not
-- consumed entirely.
readInput :: String -> Node
readInput input = node
    where
        nums = fmap read $ words input
        (node, []) = parseNode nums


-- * Implementation

scanUnfoldr :: (b -> (a,b)) -> b -> [(a,b)]
scanUnfoldr f start = res : scanUnfoldr f (snd res)
    where res = f start

sumHeaders :: Node -> Int
sumHeaders (Node [] headers) = sum headers
sumHeaders (Node nodes headers) =
    sum (sumHeaders <$> nodes) + sum headers

value :: Node -> Int
value (Node [] headers) = sum headers
value (Node nodes headers) =
    sum $ fmap (values!!) $ indices
    where
        values = value <$> nodes
        indices = fmap (\x -> x - 1) $ filter (<= length nodes) $ filter (/= 0) headers
