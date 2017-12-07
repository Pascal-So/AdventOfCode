import Data.List
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq



type NodeDesc = (String, Int, [Int])

data Node = Node String Int [Node]
data SummedNode = SummedNode String Int Int [SummedNode]

solveA :: Node -> String
solveA (Node name _ _) = name

nodeSum :: Node -> Int
nodeSum (Node _ weight children) =
    weight + sum (map nodeSum children)

{--
solveB :: Node -> Maybe Int
solveB node@(Node _ weight children) = 
    let
        adjustNode targetWeight node =
            targetWeight - sum (map nodeSum children)
    in
        case children of
            [] -> Nothing
            [a] -> solveB a
            [a,b] -> solveB a <|> solveB b <|> Just (adjustNode (nodeSum a) b)
            otherwise -> 
--}


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


main :: IO ()
main = do
    tree <- (buildTree . linkIndexes . readInput) `fmap` getContents
    print $ solveA <$> tree