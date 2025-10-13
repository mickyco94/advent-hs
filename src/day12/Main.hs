module Main where

import Data.Array
import Data.List (intersect)
import qualified Data.Set as Set

-- I have a Region Type that consists of a Char and Count
-- Or a List of Positions
--
-- Pos is a position on the map, 0-indexed
type Position = (Int, Int)

-- A region is a List of Positions and a char
data Region = Region Char [Position] deriving (Show)

type Map = Array Position Char

parse :: String -> Map
parse s = array b entries
  where
    rows = lines s
    numRows = length rows
    numCols = length (head rows)
    b = ((0, 0), (numRows - 1, numCols - 1))
    entries = [((i, j), c) | (i, row) <- zip [0 ..] rows, (j, c) <- zip [0 ..] row]

area :: Region -> Int
area (Region _ positions) = length positions

-- AAAA
-- BBCD
-- BBCC
-- EEEC
--  A way to think of it is every position add + 4, but we substract
--  if its neighbour is of the same char
perimeter :: Region -> Int
perimeter (Region _ positions) = length positions * 4 - sum (map nCount positions)
  where
    nCount p = length (positions `intersect` neighbours p)

neighbours :: Position -> [Position]
neighbours (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

-- regions finds the regions, we track a visited set and we just walk along
-- staying in the same region until we can no longer go further.
-- Either every direction is visited or is a different character
regions :: Map -> [Region]
regions m = go (0, 0) Nothing Set.empty []
  where
    go :: Position -> Maybe Char -> Set.Set Position -> [Region] -> [Region]
    go p c visited current = []
      where
        candidates = stepInRegion m p visited

stepInRegion :: Map -> Position -> Set.Set Position -> [Position]
stepInRegion m p visited = [n | n <- step m p visited, m ! p == m ! n]

step :: Map -> Position -> Set.Set Position -> [Position]
step m p visited = [n | n <- neighbours p, inRange (bounds m) n, not (Set.member n visited)]

-- fill returns a list of positions reachable from the start position
fill :: Map -> Position -> [Position]
fill m start = go start Set.empty
  where go p seen 
    | length next == 0 = Set.toList seen
   where next = stepInRegion m p 
  

main :: IO ()
main = do
  raw <- readFile "input.txt"
  print $ parse raw
