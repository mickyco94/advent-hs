module Main where

-- Input has 9 trailheads
-- A trail is a path from 0 -> 9 that can be walked in any orthogonal dir
-- The score of a trailhead is given by the number of unique trails that start from that pos
-- The solution is the sum of the trailhead scores
--
-- 89010123
-- 78121874
-- 87430965
-- 96549874
-- 45678903
-- 32019012
-- 01329801
-- 10456732
--
--
import Data.Array
import Data.List (nub)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)
import Foreign (new)

type Map = Array Pos Int

type Pos = (Int, Int)

type Path = [Pos]

readChar :: Char -> Int
readChar '.' = -1
readChar x = read [x]

parse :: String -> Map
parse s = array b entries
  where
    rows = lines s
    numRows = length rows
    numCols = length (head rows)
    b = ((0, 0), (numRows - 1, numCols - 1))
    entries = [((i, j), readChar c) | (i, row) <- zip [0 ..] rows, (j, c) <- zip [0 ..] row]

-- I don't want to find the first path, I want to find all. So I think I need a queue

neighbours :: Pos -> [Pos]
neighbours (y, x) =
  [ (yy, xx)
    | (yy, xx) <- [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
  ]

-- I probably need a visited set, or I just dedup

walk :: Map -> Pos -> [Pos]
walk m start = nub $ go [start]
  where
    go :: [Pos] -> [Pos]
    go [] = []
    go (x : xs) = x : go (xs ++ incNP)
      where
        curr = m ! x -- Current
        ibn = filter (inRange (bounds m)) (neighbours x) -- in bounds possibilites
        ibnV = map (\t -> (t, m ! t)) ibn -- in bounds possibilites values
        incN = filter (\t -> snd t == curr + 1) ibnV -- in bounds increasing values
        incNP = map fst incN -- The positions of the above

heads :: Map -> [Pos]
heads m = [p | (p, x) <- assocs m, x == 0]

fullWalks :: Map -> Pos -> Int
fullWalks m start = length dedupedwalk
  where
    w = walk m start
    only_nines = filter (\t -> (m ! t) == 9) w
    dedupedwalk = nub only_nines

solve :: String -> Int
solve s = sum walks
  where
    m = parse s
    walks = map (fullWalks m) (heads m)

main :: IO ()
main = do
  raw <- readFile "test-input.txt"
  print raw
