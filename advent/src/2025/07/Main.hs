module Main where

import Data.Array
import qualified Data.Set as Set
import MyLib (Parser, input)
import Pos (Position)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude

-- | parser interprets our input
parser :: Parser (Array Position Char)
parser = do
  l <- many (oneOf "S^.") `sepEndBy1` eol
  let entries = [((y, x), val) | (y, row) <- zip [0 ..] l, (x, val) <- zip [0 ..] row]
      numRows = length l
      numCols = length (head l)
  pure $ array ((0, 0), (numRows - 1, numCols - 1)) entries

start :: Array Position Char -> Position
start arr = head [p | (p, 'S') <- assocs arr]

splits :: Array Position Char -> [Position]
splits arr = Set.toList (go [south (start arr)] Set.empty)
  where
    go :: [Position] -> Set.Set Position -> Set.Set Position
    go [] acc = acc
    go (x : xs) acc
      | not (inRange (bounds arr) x) = go xs acc
      | x `Set.member` acc = go xs acc
      | arr ! x == '^' = go (west x : east x : xs) (Set.insert x acc)
      | otherwise = go (south x : xs) acc

timelines :: Array Position Char -> Int
timelines arr = sum [memo ! (r1, c) | c <- [c0 .. c1]]
  where
    ((_, c0), (r1, c1)) = bounds arr

    fromSides :: Position -> Int
    fromSides p = sum (map (memo !) filtered)
      where
        filtered = filter (\p' -> inRange (bounds arr) p' && arr ! p' == '^') [west p, east p]

    f (y, x)
      | (y, x) == start arr = 1
      | y == 0 = 0
      | arr ! (y - 1, x) == '^' = fromSides (y, x)
      | otherwise = memo ! (y - 1, x) + fromSides (y, x)

    memo = array (bounds arr) [(p, f p) | p <- indices arr]

south :: Position -> Position
south (y, x) = (y + 1, x)

west :: Position -> Position
west (y, x) = (y, x - 1)

east :: Position -> Position
east (y, x) = (y, x + 1)

main :: IO ()
main = do
  arr <- input 2025 7 parser
  print (length (splits arr))
  print (timelines arr)

