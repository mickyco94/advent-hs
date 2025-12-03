module Main where

import Data.Char (digitToInt)
import MyLib (Parser, input)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, eol)
import Prelude

parser :: Parser [[Int]]
parser = many (digitToInt <$> digitChar) `sepEndBy1` eol

joltage :: [Int] -> Int
joltage = foldl (\acc b -> acc * 10 + b) 0

maxbank :: [Int] -> Int -> [Int]
maxbank xs k = reverse (foldl step [] (zip xs [n, n - 1 .. 1]))
  where
    n = length xs
    step :: [Int] -> (Int, Int) -> [Int]
    step [] (curr, _) = [curr]
    step stack@(s : ss) (curr, remaining)
      | s < curr && length stack - 1 + remaining >= k = step ss (curr, remaining)
      | length stack < k = curr : stack
      | otherwise = stack

main :: IO ()
main = do
  banks <- input 2025 3 parser
  let total' n = sum [joltage (maxbank b n) | b <- banks]
  print (total' 2)
  print (total' 12)
