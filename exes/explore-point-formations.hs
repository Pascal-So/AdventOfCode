{-|
Module      : Main

Used to plot some different points in time for the
point formations in Year 2018 Day 10.

Reads the time as int from stdin and prints formation
at that time in a format that can be read by gnuplot
to stdout.
-}

module Main where

import Year2018.Day10
import Utils

main = do
    input <- getInput 2018 10
    time <- readLn
    putStrLn $ exploreData input time
