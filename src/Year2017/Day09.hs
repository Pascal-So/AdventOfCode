module Year2017.Day09 (solveA, solveB) where

import Data.List (intercalate)
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Control.Applicative

data GarbageChar = NormalChar Char | EscapedChar Char

instance Show GarbageChar where
    show (NormalChar c) = [c]
    show (EscapedChar c) = ['!', c]

data Stream = Group [Stream] | Garbage [GarbageChar]

instance Show Stream where
    show (Group streams) = "{" ++ showstreams ++ "}"
        where showstreams = intercalate "," $ map show streams
    show (Garbage g) = g >>= show

instance Read Stream where
    readsPrec _ input =
        case Parsec.parse ((,) <$> stream <*> Parsec.getInput) "" input of
            Left _ -> []
            Right result -> [result]

garbageChar :: Parsec String () GarbageChar
garbageChar = do
    esc <- Parsec.optionMaybe $ Parsec.char '!'
    case esc of
        Nothing -> NormalChar  <$> Parsec.noneOf ">"
        _       -> EscapedChar <$> Parsec.anyChar

garbage :: Parsec String () Stream
garbage = Garbage <$> (Parsec.char '<' *> Parsec.many garbageChar <* Parsec.char '>')

group :: Parsec String () Stream
group = do
    Parsec.char '{'
    contents <- stream `Parsec.sepBy` Parsec.char ','
    Parsec.char '}'
    return $ Group contents

stream :: Parsec String () Stream
stream = group <|> garbage

countGroups :: Int -> Stream -> Int
countGroups level (Garbage _) = 0
countGroups level (Group streams) =
    level + sum subgroups
    where
        subgroups = map (countGroups (level + 1)) streams

countGarbage :: Stream -> Int
countGarbage (Garbage g) = length [c | NormalChar c <- g]
countGarbage (Group gs)  = sum $ map countGarbage gs

solveA :: String -> Int
solveA = countGroups 1 . read

solveB :: String -> Int
solveB = countGarbage . read
