module Utils where

import Paths_adventofcode (getDataFileName)
import Text.Printf

getInput :: Int -> Int -> IO String
getInput year day = do
    filename <- getDataFileName $ printf "inputs/Year%04d/%02d.in" year day
    readFile filename
