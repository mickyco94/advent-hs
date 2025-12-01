{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.Unboxed
import qualified Data.Map as Map
import MyLib (Parser, input)
import Pos (Position, neighbours)
import Text.Megaparsec (oneOf, sepEndBy1, some)
import Text.Megaparsec.Char (eol)

-- | parser interprets our input
parser :: Parser (Array Position Char)
parser = do
  l <- some (oneOf "#.ES") `sepEndBy1` eol
  let entries = [((y, x), val) | (y, row) <- zip [0 ..] l, (x, val) <- zip [0 ..] row]
      numRows = length l
      numCols = length (head l)
  pure $ array ((0, 0), (numRows - 1, numCols - 1)) entries

-- | Finds the manhattan distance between two given points
mh :: Position -> Position -> Int
mh (ax, ay) (bx, by) = abs (ax - bx) + abs (by - ay)

-- Gotta be a better way to do this, this is dumb
base :: Array (Int, Int) Char -> Array Position Int
base arr = array (bounds arr) [(p, -1) | (p, _) <- assocs arr]

-- | dist produces a distance array. I don't actually need to search here. Since there will only be one path
dist :: Array Position Char -> Array (Int, Int) Int
dist arr = go [(start arr, 0)] (base arr)
  where
    go :: [(Position, Int)] -> Array Position Int -> Array Position Int
    go [] acc = acc
    go ((p, steps) : ps) acc
      | arr ! p == '#' = go ps acc
      | acc ! p <= steps && acc ! p > 0 = go ps acc
      | otherwise = go ps' acc'
      where
        ps' = map (,steps + 1) (step p arr) ++ ps
        acc' = acc // [(p, steps)]

-- | start returns the start position of the maze
start :: Array (Int, Int) Char -> Position
start maze = head [pos | (pos, 'S') <- assocs maze]

-- | cheat returns all the possible cheats of length l from a particular position
-- it includes cheats that do nothing. i.e. they don't remove any walls
cheat :: Array Position Char -> Int -> Map.Map (Position, Position) Int
cheat arr l =
  Map.fromListWith
    min
    [ ((s, e), mh s e)
      | (s, c) <- assocs arr, -- Open positions
        c /= '#',
        (e, c') <- assocs arr, -- End positions
        c' /= '#',
        mh s e <= l
    ]

-- | Takes a step that is in bounds
step :: Position -> Array (Int, Int) Char -> [Position]
step p m = [p' | p' <- neighbours p, inRange (bounds m) p']

solve :: Array (Int, Int) Char -> Int -> Int
solve arr l = length (filter (>= 100) saved)
  where
    distArr = dist arr
    cheats = cheat arr l
    saved = [distArr ! s - distArr ! e - distance | ((s, e), distance) <- Map.assocs cheats]

main :: IO ()
main = do
  arr <- input 2024 20 parser
  print (solve arr 2)
  print (solve arr 20)
