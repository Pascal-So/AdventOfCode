{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day07 (solveA, solveB) where

import PriorityQueue (PriorityQueue)
import qualified PriorityQueue as PQ
import Queue (Queue)
import qualified Queue as Q
import Utils (sortUniq)
import Data.Graph.Inductive
import Data.List (foldl')
import Data.Char (ord, chr)

readInput :: String -> [Edge]
readInput input = do
    line <- lines input
    return (ord $ line !! 5, ord $ line !! 36)

extractNodes :: [Edge] -> [Node]
extractNodes edges =
    sortUniq $ fmap fst edges ++ fmap snd edges

lowestToposort :: (Graph gr) => gr a b -> PriorityQueue Node -> [Node]
lowestToposort graph queue =
    case PQ.minView queue of
        Just (node, rest) ->
            node : lowestToposort graph' queue'
            where
                graph' = delNode node graph
                queue' = PQ.union rest $ PQ.fromList $ filter (null . pre graph') $ suc graph node
        Nothing -> []

startqueue :: [Node] -> [Edge] -> PriorityQueue Node
startqueue nodes edges = PQ.fromList $ filter (`notElem` dependent) nodes
    where
        dependent = sortUniq $ fmap snd edges

solveA :: String -> String
solveA input = map chr $ lowestToposort graph $ startqueue nodes edges
    where
        edges = readInput input
        nodes = extractNodes edges
        graph :: Gr () ()
        graph = mkUGraph nodes edges

duration :: Node -> Int
duration n = 60 + n - ord 'A' + 1

solveWithWorkers :: (Graph gr) => Int -> gr a b -> PriorityQueue Node -> PriorityQueue (Int, Node) -> Int
solveWithWorkers idleworkers graph taskqueue workerqueue =
    if idleworkers > 0 then
        case PQ.minView taskqueue of
            Just (task, taskqueue') -> startTask task taskqueue'
            Nothing                 -> finishTask
    else
        finishTask
    where
        startTask task taskqueue' =
            solveWithWorkers (idleworkers - 1) graph taskqueue' workerqueue'
                where
                    workerqueue' = PQ.add (duration task, task) workerqueue

        finishTask =
            case PQ.minView workerqueue of
                Just ((timePassed, task), workerqueue') ->
                    timePassed + solveWithWorkers (idleworkers + 1) graph' taskqueue' (PQ.map (\(d,n) -> (d-timePassed,n)) workerqueue')
                    where
                        graph' = delNode task graph
                        taskqueue' = PQ.union taskqueue $ PQ.fromList $ filter (null . pre graph') $ suc graph task
                Nothing -> 0

solveB :: String -> Int
solveB input = solveWithWorkers 5 graph tasks workers
    where
        edges = readInput input
        nodes = extractNodes edges
        graph :: Gr () ()
        graph = mkUGraph nodes edges
        tasks = startqueue nodes edges
        workers = PQ.empty
