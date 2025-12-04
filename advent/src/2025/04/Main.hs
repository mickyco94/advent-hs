module Main where

import Data.Array
import Data.Function ((&))
import MyLib (Parser, input)
import Pos (Position, moore)
import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import Prelude

parser :: Parser (Array Position Char)
parser = do
  l <- some (oneOf ".@") `sepEndBy1` eol
  let entries = [((y, x), val) | (y, row) <- zip [0 ..] l, (x, val) <- zip [0 ..] row]
      numRows = length l
      numCols = length (head l)
  pure $ array ((0, 0), (numRows - 1, numCols - 1)) entries

-- | access returns the positions of accessibly paper rolls
access :: Array Position Char -> [Position]
access arr = [p | (p, '@') <- assocs arr, length (free p) < 4]
  where
    free p = [m | m <- moore p, inRange (bounds arr) m, arr ! m == '@']

-- | step builds up the next version of the array, tallying the total
step :: (Array Position Char, Int) -> (Array Position Char, Int)
step (arr, n) = (arr // replace, n + length a)
  where
    a = access arr
    replace = [(p, '.') | p <- a]

main :: IO ()
main = do
  arr <- input 2025 4 parser
  (arr, 0) & step &  snd & print
  (arr, 0) & until (null . access . fst) step & snd & print
