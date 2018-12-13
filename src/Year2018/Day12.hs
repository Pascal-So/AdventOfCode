{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day12 where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Data.List (sort)
import Data.Function (on)

import Debug.Trace

-- | The rules are indexed by converting their source pattern from
-- a binary representation with the first digit being the MSB to
-- their index in the vector.
type Rule = Vector Bool
-- | The state lists all the pots (True means a plant is present),
-- together with the index of the first pot in the list.
type State = ([Bool], Int)

-- * Solutions

solveA :: String -> Int
solveA input = sumPotIndices endstate
    where
        initstate :: State
        (initstate, rule) = parseInput input
        states = iterate (generation rule) initstate
        endstate = states !! 20

-- | This assumes that the state only consists of a single non-changing shuttle after a not too long time.
solveB :: String -> Integer
solveB input = sumPotIndices endstate
    where
        initstate :: State
        (initstate, rule) = parseInput input
        states = iterate (generation rule) initstate
        idx = firstDuplicateIndex states
        reststates = drop idx states
        (dupState, dupStateShift) = head reststates
        velocity = snd (reststates !! 1) - dupStateShift
        endindex = fromIntegral dupStateShift + (fromIntegral velocity) * (50000000000 - fromIntegral idx)
        endstate = (dupState, endindex)

-- * Parsing

-- | Expected format:
--
-- * Initial state start after 15 characters.
-- * Rules start on the third line.
-- * Rules are in any order.
-- * A rule looks like this: "..### => ."
-- * . means False and # means True
parseInput :: String -> (State, Rule)
parseInput input = ((padding ++ pots ++ padding, - length padding), rule)
    where
        padding = replicate 4 False
        tobool = (== '#')
        ls = lines input
        pots = tobool <$> (drop 15 $ head ls)
        rule = Vec.fromList $ reverse $ (tobool . last) <$> (sort $ drop 2 ls)

-- * Implementation

-- | Sum the indices of positions where a plant is alive.
-- >>> sumPotIndices ([False,True,True],-3)
-- -3
sumPotIndices :: (Integral a) => ([Bool], a) -> a
sumPotIndices (pots, shift) =
    sum [idx + shift | (True, idx) <- zip pots [0..]]

-- | Run the simulation for a single generation
generation :: Rule -> State -> State
generation rule (pots, shift) = trim 4 (go pots 0, shift-2)
    where
        go :: [Bool] -> Int -> [Bool]
        go [] lastidx
            | lastidx >= 16 || lastidx == 0 = []
            | otherwise =
                (rule ! idx) : go [] idx
                where
                    idx = (lastidx * 2) `mod` 32
        go (p:ps) lastidx =
            (rule ! idx) : go ps idx
            where
                idx = ((lastidx * 2) `mod` 32) + if p then 1 else 0

-- | Prepend a value n times where n can also be negative. For negative n,
-- `-n` values are removed from the beginning of the list.
--
-- >>> prependValues (-1) 'x' "abc"
-- "bc"
--
-- >>> prependValues 0 'x' "abc"
-- "abc"
--
-- >>> prependValues 2 'x' "abc"
-- "xxabc"
prependValues :: Int  -- ^ n
              -> a    -- ^ value to prepend
              -> [a]  -- ^ input list
              -> [a]
prependValues n val
    | n < 0     = drop (-n)
    | otherwise = (++) (replicate n val)

-- | Sets the amount of falses to exactly n on both sides and adjusts
-- the shift value accordingly.
--
-- >>> trim 3 ([False,False,True],-5)
-- ([False,False,False,True,False,False,False],-6)
--
-- >>> trim 1 ([False,False,True],1)
-- ([False,True,False],2)
trim :: Int    -- ^ Nr of leading/trailing falses
     -> State  -- ^ input state
     -> State
trim n (pots, shift) = (pots', shift - shiftdist)
    where
        shiftdist = n - (length $ takeWhile not pots)
        rev = reverse pots
        shiftdist_end = n - (length $ takeWhile not rev)
        pots' = prependValues shiftdist False $ reverse $ prependValues shiftdist_end False rev

-- | 0 based index of the first of two consecutive states that show the same
-- pattern but not neccessarily at the same starting index.
firstDuplicateIndex :: [State] -> Int
firstDuplicateIndex ((astate,_) : b@(bstate,_) : xs) =
    if astate == bstate
        then 0
        else 1 + firstDuplicateIndex (b:xs)

-- * Debugging Helpers

-- | Show the state as it is displayed in the task description.
--
-- The first argument is the coordinate of the first shown character,
-- and can be used to set a sufficient amount of padding.
showState :: Int -> State -> String
showState n (state, idx) = prependValues (idx - n) '.' nopadding
    where
        nopadding = fmap (\p -> if p then '#' else '.') state
