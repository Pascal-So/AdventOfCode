module Year2017.Day20 where

import qualified Data.List as List
import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec)
import qualified Data.Either as Either
import Data.Ratio
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Function
import Debug.Trace

type Pos = [Int]
data Particle = Particle { _position     :: Pos
                         , _velocity     :: Pos
                         , _acceleration :: Pos
                         } deriving (Show)

data Quadratic = Quadratic { _squared  :: Ratio Int
                           , _linear   :: Ratio Int
                           , _constant :: Ratio Int
                           } deriving (Show)

qDiff :: Quadratic -> Quadratic -> Quadratic
qDiff q1 q2 =
    Quadratic (_squared q1 - _squared q2) (_linear q1 - _linear q2) (_constant q1 - _constant q2)

ratIsSquare :: Ratio Int -> Bool
ratIsSquare r =
    isSquare (denominator r) && isSquare (numerator r)
    where
        isSquare n =
            sq * sq == n
            where sq = floor $ sqrt $ (fromIntegral n::Double)

ratDiv :: Ratio Int -> Ratio Int -> Ratio Int
ratDiv a b =
    a * ratInv b
    where
        ratInv r =
            denominator r % numerator r

ratIsInteger :: Ratio Int -> Bool
ratIsInteger r =
    denominator r == 1

ratSqrt :: Ratio Int -> Ratio Int
ratSqrt rat =
    sr (numerator rat) % sr (denominator rat)
    where
        sr n = floor $ sqrt $ (fromIntegral n::Double)

collisions :: Quadratic -> Quadratic -> ([Int], Bool)
collisions q1 q2 =
    if disc < 0 || not (ratIsSquare disc) then
        ([], False)
    else if a == 0 && b == 0 && c == 0 then
        ([0], True)
    else if a == 0 && b == 0 then
        ([], False)
    else if a == 0 && not (ratIsInteger $ ratDiv c b) then
        ([], False)
    else if a == 0 then
        ([numerator $ ratDiv (-c) b], False)
    else
        let root = ratSqrt disc
            solutions_db = [-b + root, -b - root]
        in
            (map head $ List.group $ map numerator $ filter ratIsInteger $ map (\x -> ratDiv x (a * 2)) solutions_db, False)
    where
        Quadratic a b c = qDiff q1 q2
        disc = b * b - 4 * a * c

combineSegments :: ([Int],Bool) -> ([Int], Bool) -> ([Int], Bool)
combineSegments (points1, const1) (points2, const2) =
    if const1 && const2 then
        ([0], True)
    else if const1 then
        (points2, False)
    else if const2 then
        (points1, False)
    else
        (List.intersect points1 points2, False)

trajecoryCollisions :: [Quadratic] -> [Quadratic] -> [Int]
trajecoryCollisions qs1 qs2 =
    segToList $ List.foldl1 combineSegments $ map (uncurry collisions) $ zip qs1 qs2
    where
        segToList (_, True) = [0]
        segToList (lst, _)  = lst

pvaToQuadratic :: Int -> Int -> Int -> Quadratic
pvaToQuadratic p v a =
    Quadratic s l c
    where
        s = a % 2
        l = (fromIntegral v) + s
        c = fromIntegral $ p

getTrajectory :: Particle -> [Quadratic]
getTrajectory part =
    map (uncurry (uncurry pvaToQuadratic)) $ zip (zip (_position part) (_velocity part)) (_acceleration part)

allCollisions :: [Particle] -> [(Int, [Int])]
allCollisions parts =
    combine $ collideOthers =<< (tail $ zip [0..] parts)
    where
        combine :: [(Int, [Int])] -> [(Int, [Int])]
        combine =
            map (\ps -> (fst $ head ps, List.foldl' List.union [] $ map snd ps)) . List.groupBy ((==) `on` fst) . List.sort
        collide2 :: (Int, Particle) -> (Int, Particle) -> [(Int, [Int])]
        collide2 (idx1, part1) (idx2, part2) =
            map (\time -> (time, [idx1, idx2])) $ (trajecoryCollisions `on` getTrajectory) part1 part2
        collideOthers :: (Int, Particle) -> [(Int, [Int])]
        collideOthers p@(idx, part) =
            collide2 p =<< (take idx $ zip [0..] parts)

solveA :: [Particle] -> Int
solveA parts =
    snd $ head $ List.sort $ zip (map (sum . map abs . _acceleration) parts) [0..]

solveB :: [Particle] -> Int
solveB parts =
    Seq.length $ Seq.filter id $ List.foldl' eliminate start $ dropWhile (\(a,_) -> a < 0) $ allCollisions parts
    where
        start = Seq.fromList $ replicate (length parts) True
        eliminate :: Seq Bool -> (Int, [Int]) -> Seq Bool
        eliminate alive (_,colliding) =
            if nrAlive > 1 then
                foldr ($) alive $ map (\x -> Seq.update x False) colliding
            else
                alive
            where
                nrAlive = length $ filter id $ map (Seq.index alive) colliding

parseInt :: Parsec String () Int
parseInt = do
    Parsec.spaces
    minus <- Parsec.optionMaybe (Parsec.char '-')
    num <- read <$> Parsec.many1 (Parsec.oneOf ['0'..'9'])
    Parsec.spaces
    return $ case minus of
        Nothing -> num
        Just _  -> -num

parseComponent :: Parsec String () Pos
parseComponent = do
    Parsec.spaces
    Parsec.anyChar
    Parsec.char '='
    Parsec.char '<'
    Parsec.spaces
    p <- Parsec.sepBy parseInt (Parsec.char ',')
    Parsec.char '>'
    Parsec.spaces
    return p

parseParticle :: Parsec String () Particle
parseParticle = do
    Parsec.spaces
    [p,v,a] <- Parsec.sepBy parseComponent (Parsec.char ',')
    return $ Particle p v a

readParticles :: String -> [Particle]
readParticles str =
    Either.rights $ map (Parsec.parse parseParticle "") $ lines str

main = do
    input <- readParticles <$> getContents
    print $ solveB input