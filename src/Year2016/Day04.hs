module Year2016.Day04 (solveA, solveB) where

import Data.Char (ord, chr)
import Data.Function
import Data.List

data Room = Room String Int String deriving (Show)

parseRoom :: String -> Room
parseRoom str =
    Room (init name) (read id) (tail $ init check)
    where
        (name, rest) = break (`elem` ['0'..'9']) str
        (id, check) = break (== '[') rest


rle :: (Eq a) => [a] -> [(a, Int)]
rle = map (\l -> (head l, length l)) . group

checksum :: String -> String
checksum str =
    take 5 . map fst . sortBy (flip compare `on` snd) . sortOn fst $ runlen
    where runlen = rle . sort $ filter (/= '-') str

realRoom :: Room -> Bool
realRoom (Room name _ check) =
    checksum name == check

rotN :: Int -> Char -> Char
rotN n c =
    chr $ rotated + ord 'a'
    where
        orig = ord c - ord 'a'
        rotated = (orig + n) `mod` 26

decrypt :: Room -> String
decrypt (Room name id _) =
    map dec name
    where
        dec c = if c == '-' then ' ' else rotN id c

readInput :: String -> [Room]
readInput = map parseRoom . lines

solveA :: String -> Int
solveA = sum . map (\(Room _ id _) -> id) . filter realRoom . readInput

solveB :: String -> Int
solveB input = roomId
    where
        rooms = readInput input
        realRooms = filter realRoom rooms
        Just (Room _ roomId _) = find (\r -> decrypt r == "northpole object storage") realRooms
