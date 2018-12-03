module Year2017.Day07 where

import Data.List
import Data.Foldable
import Data.Maybe
import Data.Function
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type NodeDesc = (String, Int, [Int])

data Node = Node String Int [Node] deriving (Show)

nodeSum :: Node -> Int
nodeSum (Node _ weight children) =
    weight + sum (map nodeSum children)

findRoot :: Seq NodeDesc -> Maybe Int
findRoot nodes =
    Seq.elemIndexL False $ foldl (\acc id -> Seq.update id True acc) init lst
    where
        init = Seq.fromList $ replicate (length nodes) False
        lst = nodes >>= (\(_,_,children) -> Seq.fromList children)

buildTree :: Seq NodeDesc -> Maybe Node
buildTree nodes =
    tree <$> findRoot nodes
    where
        tree pos = Node name weight $ map tree children
            where (name, weight, children) = Seq.index nodes pos

linkIndexes :: Seq (String, Int, [String]) -> Seq NodeDesc
linkIndexes nodes =
    fmap (\(name, weight, children) -> (name, weight, catMaybes $ map (flip Map.lookup nameMap) children)) nodes
    where
        nameMap = Map.fromList $ zip (map (\(s,_,_) -> s) $ toList nodes) [0..]

readInput :: String -> Seq (String, Int, [String])
readInput str =
    Seq.fromList $ map (parseNode . words) ls
    where
        ls = lines str
        parseNode [name, s_weigh] =
            (name, read $ tail $ init s_weigh, [])
        parseNode (name:s_weigh:_:xs) =
            (name, weight, children)
            where
                weight = read $ tail $ init s_weigh
                children = map (takeWhile (flip elem ['a'..'z'])) xs

solveA :: Node -> String
solveA (Node name _ _) = name

-- The case branching lower down is still bad, but it can be simplified a lot by the
-- assumption that there will be exactly one node with an inorrect weight
solveB :: Node -> Maybe Int
solveB = go EQ
    where
        go ord node@(Node name weight children) =
            let
                correctedNodeWeight targetWeight (Node name _ children) =
                    targetWeight - sum (map nodeSum children)
                sorted = sortBy (compare `on` nodeSum) children
            in
                case (sorted, ord) of
                    ([],_) -> Nothing
                    ([a],_) -> go ord a
                    ([a,b],EQ) -> go LT a <|> go GT b
                    ([a,b],_) ->
                        if nodeSum a == nodeSum b then
                            Nothing
                        else
                            case ord of
                                EQ -> go LT a <|> go GT b <|> (Just $ correctedNodeWeight (nodeSum a) b)
                                GT -> go GT b <|> (Just $ correctedNodeWeight (nodeSum a) b)
                                LT -> go LT a <|> (Just $ correctedNodeWeight (nodeSum b) a)
                    ((f:s:_),_) ->
                        let adjustNode = if nodeSum f == nodeSum s then last sorted else f
                        in
                            if nodeSum adjustNode == nodeSum s then
                                Nothing
                            else
                                go ((compare `on` nodeSum) adjustNode s) adjustNode
                                    <|> (Just $ correctedNodeWeight (nodeSum s) adjustNode )

main :: IO ()
main = do
    tree <- (buildTree . linkIndexes . readInput) `fmap` getContents
    print $ solveA <$> tree
