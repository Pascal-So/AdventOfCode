module Year2018.Day03 where

import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

-- type Parser = Parsec Void String

-- symbol :: String -> Parser String
-- symbol = L.symbol space1

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme space1

-- data Rectangle = Rectangle { left :: Int
--                            , top :: Int
--                            , width :: Int
--                            , height :: Int
--                            } deriving (Show)

-- parseLine :: Parser (Int, Rectangle)
-- parseLine = do
--     symbol "#"
--     i <- lexeme L.decimal
--     symbol "@"
--     l <- lexeme L.decimal
--     symbol ","
--     t <- lexeme L.decimal
--     symbol ":"
--     w <- lexeme L.decimal
--     symbol "x"
--     h <- lexeme L.decimal

--     return (i, Rectangle l t w h)
