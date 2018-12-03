module Year2018.Day03 (solveA, solveB) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepEndBy1, eof)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error

type Parser = Parsec Void String

number :: Parser Int
number = L.decimal

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

data Rectangle = Rectangle { left :: Int
                           , top :: Int
                           , width :: Int
                           , height :: Int
                           } deriving (Show)

parseLine :: Parser (Int, Rectangle)
parseLine = do
    char '#'
    i <- number
    space1
    char '@'
    space1
    l <- number
    char ','
    t <- number
    char ':'
    space1
    w <- number
    char 'x'
    h <- number

    return (i, Rectangle l t w h)

readInput :: String -> [(Int, Rectangle)]
readInput input =
    case parse ((sepEndBy1 parseLine newline) <* eof) "problem input" input of
        Left err  -> error $ parseErrorPretty err
        Right res -> res

intersect :: Rectangle -> Rectangle -> Bool
intersect r1 r2 =
    if left r1 > left r2 then
        intersect r2 r1
    else
        left r1 + width r1 > left r2  &&
        top r1 + height r1 > top r2 &&
        top r2 + height r2 > top r1

solveA :: String -> Int
solveA input = 0

solveB :: String -> Int
solveB input = fst . head $ filter good rects
    where
        rects = readInput input
        except n = filter (\(i, r) -> i /= n)
        good (idx, rect) = all (not . intersect rect) . map snd $ except idx rects