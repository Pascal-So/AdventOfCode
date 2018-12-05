module Year2018.Day04 (solveA, solveB) where

import Data.List (group, groupBy, sort, sortBy)
import Data.Function (on)

type Guard = Int

data Event = GuardStart Guard | FallAsleep | WakeUp deriving (Eq, Show)

parseLine :: String -> (Int, Event)
parseLine line =
    (minute, event)
    where
        minute = read $ take 2 $ drop 15 line
        event = case drop 19 line of
            "wakes up" -> WakeUp
            "falls asleep" -> FallAsleep
            guardstr -> GuardStart . read . head . words $ drop 7 guardstr

readInput :: String -> [(Int, Event)]
readInput = map parseLine . sort . lines

sumDiffs :: [Int] -> Int
sumDiffs (x:y:ys) = y - x + sumDiffs ys
sumDiffs [] = 0

extractSleepPhases :: [(Int, Event)] -> [(Guard, Int, Int)]
extractSleepPhases [] = []
extractSleepPhases ((_, GuardStart guard) : xs) =
    sleepPhases shift ++ extractSleepPhases rest
    where
        partOfShift (_, ev) = ev `elem` [FallAsleep, WakeUp]
        shift = takeWhile partOfShift xs
        rest = dropWhile partOfShift xs
        sleepPhases [] = []
        sleepPhases (start:end:morePhases) =
            (guard, fst start, fst end) : sleepPhases morePhases

extractSleep :: [(Int, Event)] -> [(Guard, Int)]
extractSleep [] = []
extractSleep ((_, GuardStart guard) : xs) =
    (guard, sleep) : extractSleep rest
    where
        shift = takeWhile (\(_, ev) -> ev `elem` [FallAsleep, WakeUp]) xs
        rest = dropWhile (\(_, ev) -> ev `elem` [FallAsleep, WakeUp]) xs
        sleep = sumDiffs $ map fst shift

sumSleep :: [(Guard, Int, Int)] -> [(Guard, Int)]
sumSleep =
    map (\lst -> (fst $ head lst, sum $ map snd lst)) . groupBy ((==) `on` fst) . sort . map (\(g,s,e) -> (g, e-s))

sleepiestGuard :: [(Guard, Int, Int)] -> Guard
sleepiestGuard = fst . last . sortBy (compare `on` snd) . sumSleep

sleepiestMinute :: [(Guard, Int, Int)] -> Guard -> (Int, Int)
sleepiestMinute phases guard =
    maximum $ scanl (\(summed, _) (minute, diff) -> (summed + diff, minute)) (0,0) $ sort borders
    where
        listBorders (_, s, e) = [(s, 1), (e, -1)]
        borders = filter (\(g,_,_) -> g == guard) phases >>= listBorders

extractGuards :: [(Int, Event)] -> [Guard]
extractGuards events =
    map head . group $ sort [g | GuardStart g <- map snd events]

solveA :: String -> Int
solveA input = guard * minute
    where
        phases = extractSleepPhases $ readInput input
        guard = sleepiestGuard phases
        (_, minute) = sleepiestMinute phases guard

solveB :: String -> Int
solveB input = guard * minute
    where
        events = readInput input
        phases = extractSleepPhases events
        perGuard = map (\g -> (g, sleepiestMinute phases g)) $ extractGuards events
        (guard, (_, minute)) = last $ sortBy (compare `on` (fst . snd)) perGuard
