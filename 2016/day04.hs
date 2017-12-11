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

solveA :: [Room] -> Int
solveA = sum . map (\(Room _ id _) -> id) . filter realRoom

solveB :: [Room] -> Maybe Room
solveB = find (\r -> decrypt r == "northpole object storage") . filter realRoom

main = do
    rooms <- map parseRoom . lines <$> getContents
    print $ solveB rooms