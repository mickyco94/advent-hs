module MyLib where

import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Char (space)
import Text.Printf (printf)

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
