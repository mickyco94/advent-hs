module Main where

-- 89010123
-- 78121874
-- 87430965
-- 96549874
-- 45678903
-- 32019012
-- 01329801
-- 10456732

import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map

type Grid = Array Pos Int

type Pos = (Int, Int)

type Path = [Pos]

readChar :: Char -> Int
readChar '.' = -1
readChar x = read [x]

parse :: String -> Grid
parse s = array b entries
  where
    rows = lines s
    numRows = length rows
    numCols = length (head rows)
    b = ((0, 0), (numRows - 1, numCols - 1))
    entries = [((i, j), readChar c) | (i, row) <- zip [0 ..] rows, (j, c) <- zip [0 ..] row]

neighbours :: Pos -> [Pos]
neighbours (y, x) = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

walk :: Grid -> Pos -> Map Pos Int
walk m start = foldl (step m) (Map.singleton start 1) [1 .. 9]

step :: Grid -> Map Pos Int -> Int -> Map Pos Int
step g m target =
  Map.fromListWith
    (+)
    [ (neighbour, n)
      | (y, n) <- Map.assocs m,
        neighbour <- neighbours y,
        next <- idx g neighbour,
        next == target
    ]

idx :: Grid -> Pos -> [Int]
idx m p = [m ! p | inRange (bounds m) p]

heads :: Grid -> [Pos]
heads m = [p | (p, x) <- assocs m, x == 0]

paths :: Grid -> [Map Pos Int]
paths g = [walk g start | start <- heads g]

rating :: Map Pos Int -> Int
rating acc = sum $ Map.elems acc

score :: Map Pos Int -> Int
score acc = length $ Map.elems acc

totalScore :: Grid -> Int
totalScore g = sum (map score (paths g))

totalRating :: Grid -> Int
totalRating g = sum (map rating (paths g))

solve :: String -> Int
solve s = sum (map sum (paths m))
  where
    m = parse s

main :: IO ()
main = do
  raw <- readFile "test-input.txt"
  print $ solve raw
