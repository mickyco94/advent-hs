module Main where

import Data.Array
import Data.Char (digitToInt, isDigit)
import Data.List (transpose)
import MyLib (Parser, input)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, space)
import Prelude

data Op = Mul | Add deriving (Show, Eq)

parser :: Parser (Array (Int, Int) Char, [Op])
parser = do
  nums <- parseArr
  ops <- opp `sepEndBy1` space
  pure (nums, ops)
  where
    opp = do
      c <- oneOf ['*', '+']
      case c of
        '*' -> pure Mul
        '+' -> pure Add
        _ -> fail "Unexpected char"

parseArr :: Parser (Array (Int, Int) Char)
parseArr = do
  l <- some (digitChar <|> char ' ') `sepEndBy1` eol
  let entries = [((y, x), val) | (y, row) <- zip [0 ..] l, (x, val) <- zip [0 ..] row]
      numRows = length l
      numCols = length (head l)
  pure $ array ((0, 0), (numRows - 1, numCols - 1)) entries

apply :: Op -> [Int] -> Int
apply Mul = product
apply Add = sum

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc t -> acc * 10 + t) 0

colmajor :: Array (Int, Int) a -> [[a]]
colmajor arr =
  [ [ arr ! (r, c)
      | r <- [r0 .. r1]
    ]
    | c <- [c0 .. c1]
  ]
  where
    ((r0, c0), (r1, c1)) = bounds arr

rowmajor :: Array (Int, Int) a -> [[a]]
rowmajor arr =
  [ [ arr ! (r, c)
      | c <- [c0 .. c1]
    ]
    | r <- [r0 .. r1]
  ]
  where
    ((r0, c0), (r1, c1)) = bounds arr

colwise :: Array (Int, Int) Char -> [[Int]]
colwise arr = splitOn (== 0) asInts
  where
    cols = colmajor arr
    justdigits = map (filter isDigit) cols
    asDigits = map (map digitToInt) justdigits
    asInts = map digitsToInt asDigits

rowwise :: Array (Int, Int) Char -> [[Int]]
rowwise arr = transpose asInts
  where
    rows = rowmajor arr
    grouped = map (splitOn (== ' ')) rows
    asDigits = map (map (map digitToInt)) grouped
    asInts = map (map digitsToInt) asDigits

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs =
  case dropWhile p xs of
    [] -> []
    ys -> pre : splitOn p rest
      where
        (pre, rest) = break p ys

main :: IO ()
main = do
  (arr, ops) <- input 2025 6 parser
  print (sum (zipWith apply ops (rowwise arr)))
  print (sum (zipWith apply ops (colwise arr)))
