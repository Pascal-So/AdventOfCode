module Year2018.Day03 (solveA, solveB) where

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepEndBy1, eof)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error

import PrefixSumGrid (PrefixSumGrid)
import qualified PrefixSumGrid as PSG
import Data.List (foldl')

import Debug.Trace

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

gridSize :: [Rectangle] -> (Int, Int)
gridSize [] = (0, 0)
gridSize (Rectangle l t w h : rest) =
    (max (l + w) rx, max (t + h) ry)
    where
        (rx, ry) = gridSize rest

solveA :: String -> Int
solveA input = length $ filter (>= (2 :: Int)) $ concat $ PSG.evaluate finalPrefixGrid
    where
        rects = map snd $ readInput input
        (gridWidth, gridHeight) = gridSize rects
        initPrefixGrid = PSG.generate gridWidth gridHeight :: PrefixSumGrid Int
        finalPrefixGrid = foldl' (\grid (Rectangle l t w h) -> PSG.addRectangle (l,t) (w,h) 1 grid) initPrefixGrid rects

solveB :: String -> Int
solveB input = fst . head $ filter good rects
    where
        rects = readInput input
        except n = filter (\(i, r) -> i /= n)
        good (idx, rect) = all (not . intersect rect) . map snd $ except idx rects