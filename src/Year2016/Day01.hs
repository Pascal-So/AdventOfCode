module Year2016.Day01 (solveA, solveB) where

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Set as Set
import Prelude hiding (Right, Left)

import Pos

type Heading = Int
data Turn = Left | Right deriving (Show, Eq)
type Instruction = (Turn, Int)

readInstructions :: String -> [Instruction]
readInstructions str =
    map readSingleDir $ map (takeWhile (/= ',')) $ words str
    where
        readSingleDir ('R':dist) = (Right, read dist)
        readSingleDir ('L':dist) = (Left, read dist)

turn :: Turn -> Heading -> Heading
turn t h =
    (h + (if t == Right then 1 else -1) + 4) `mod` 4

headingStep :: Heading -> Pos
headingStep 0 = (0,1)
headingStep 1 = (1,0)
headingStep 2 = (0,-1)
headingStep 3 = (-1,0)

walkStraight :: Heading -> Int -> Pos -> Pos
walkStraight h dist p =
    addP p $ scaleP dist $ headingStep h

followInstruction :: (Pos, Heading) -> Instruction -> (Pos, Heading)
followInstruction (p,h) (t,dist) =
    let
        newh = turn t h
    in
        (walkStraight newh dist p, newh)

segmentContains :: (Pos, Heading, Int) -> Pos -> Bool
segmentContains (a, heading, len) b =
    walkStraight heading dist a == b
    where
        dist = min len $ manhattan a b

-- returns first intersecting point in segment b
segmentIntersection :: (Pos, Heading, Int) -> (Pos, Heading, Int) -> Maybe Pos
segmentIntersection a@(aPos, aHeading, aDist) b@(bPos, bHeading, bDist) =
    if (aHeading + bHeading) `mod` 2 == 0 then -- parallel
        let bEnd = walkStraight bHeading (bDist +1) bPos
            aEnd = walkStraight aHeading (aDist +1) aPos
        in  if segmentContains a bPos then
                Just bPos
            else if segmentContains b aPos || segmentContains b aEnd then
                if aHeading == bHeading then Just aPos else Just aEnd
            else
                Nothing
    else -- perpendicular
        let ((horP,horH,horD), (verP,verH,verD)) = if aHeading `mod` 2 == 0 then (b,a) else (a,b)
            dx = min (abs $ fst aPos - fst bPos) horD
            dy = min (abs $ snd aPos - snd bPos) verD
            p1 = walkStraight horH dx horP
            p2 = walkStraight verH dy verP
        in
            if p1 == p2 then Just p1 else Nothing

mapRest :: (a -> a -> b) -> [a] -> [b]
mapRest f [] = []
mapRest f (x:xs) = map (f x) xs

firstIntersection :: [(Pos, Heading, Int)] -> Maybe Pos
firstIntersection lst =
    listToMaybe . catMaybes $ tails >>= mapRest segmentIntersection
    where
        tails = take (length lst) $ iterate tail lst



solveA :: String -> Int
solveA = manhattan (0,0) . fst . foldl' followInstruction ((0,0), 0) . readInstructions

solveB :: String -> Maybe Int
solveB input = manhattan (0,0) <$> firstIntersection all
    where
        inst = readInstructions input
        (positions, _:headings) = unzip (scanl followInstruction ((0,0), 0) inst)
        all = zip3 positions headings $ map ((\x -> x- 1) . snd) inst
