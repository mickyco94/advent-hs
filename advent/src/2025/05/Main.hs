module Main where

import Data.List (sort)
import MyLib (Parser, input, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, newline)
import Prelude

-- Range is upper and lower inclusive
type Range = (Int, Int)

parser :: Parser ([Range], [Int])
parser = do
  r <- range `sepEndBy1` eol
  _ <- newline
  ints <- integer `sepEndBy1` eol
  pure (r, ints)

range :: Parser Range
range = do
  start <- integer
  _ <- char '-'
  end <- integer
  pure (start, end)

inRange :: Range -> Int -> Bool
inRange (lower, upper) n = lower <= n && n <= upper

toList :: Range -> [Int]
toList (a, b) = [a .. b]

size :: Range -> Int
size (a, b) = b - a + 1

merge :: [Range] -> [Range]
merge xs = foldr step [] (sort xs)
  where
    step interval [] = [interval]
    step (s1, e1) acc@((s2, e2) : rest)
      | e1 < s2 = (s1, e1) : acc
      | otherwise = (s1, max e1 e2) : rest

main :: IO ()
main = do
  (ranges, nums) <- input 2025 5 parser
  print (length [n | n <- nums, any (`inRange` n) ranges])
  print ((sum . map size . merge) ranges)
