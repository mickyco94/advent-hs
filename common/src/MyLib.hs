module MyLib where

import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Data.Void (Void)
import Text.Printf (printf)

type Parser = Parsec Void String

-- | input reads the input using the defined parser function
input :: Int -> Int -> Parser a -> IO a
input year day parser = do
  contents <- readFile $ printf "%d/%02d.txt" year day
  case parse parser "input.txt" contents of
    Left err -> error $ errorBundlePretty err
    Right r -> return r
