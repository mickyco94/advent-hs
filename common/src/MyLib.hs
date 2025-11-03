module MyLib where

import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec (Parsec, errorBundlePretty, parse, empty)
import Text.Megaparsec.Char (space, space1)
import Text.Printf (printf)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | input reads the input using the defined parser function
-- | for the given year and day
input :: Int -> Int -> Parser a -> IO a
input year day parser = do
  contents <- readFile $ printf "inputs/%d/%02d.txt" year day
  case parse parser "input.txt" contents of
    Left err -> error $ errorBundlePretty err
    Right r -> return r

lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- Define how to skip spaces and comments
spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1 -- whitespace (space, tab, newline)
    empty -- no line comments
    empty -- no block comments

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integer :: Parser Int
integer = L.signed spaceConsumer L.decimal


