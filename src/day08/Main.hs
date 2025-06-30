{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import Data.List (nub, sort, tails)
import System.Environment (getArgs)

-- ............
-- ........0...
-- .....0......
-- .......0....
-- ....0.......
-- ......A.....
-- ............
-- ............
-- ........A...
-- .........A..
-- ............
-- ............
--
-- Need to find the location of all antinodes
-- In the map and deduplicate
-- An antenna is any char in the input that is not a .
--
-- Construct a Grid
-- function to get positions of an Antenna of a particular type
--
-- From antenna positions I need to calculate antinode positions
-- An antinode position is an (x,y) that is dist X from one antenna
-- and dist 2X from another antenna
type Position = (Int, Int)

type Vec = (Int, Int)

type Map = Array Position Char

parse :: String -> Map
parse s = array b entries
  where
    rows = lines s
    numRows = length rows
    numCols = length (head rows)
    b = ((0, 0), (numRows - 1, numCols - 1))
    entries = [((i, j), c) | (i, row) <- zip [0 ..] rows, (j, c) <- zip [0 ..] row]

readInput :: IO String
readInput = do
  args <- getArgs
  case args of
    [path] -> readFile path
    _ -> error "no input"

antennas :: Map -> [(Position, Char)]
antennas m = [((i, j), c) | ((i, j), c) <- assocs m, c /= '.']

vec :: Position -> Position -> Vec
vec (i, j) (ii, jj) = (ii - i, jj - j)

add :: Position -> Vec -> Position
add (i, j) (dx, dy) = (i + dx, j + dy)

neg :: Vec -> Vec
neg (i, j) = (-i, -j)

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

pairedAntennas :: Map -> [(Position, Position, Char)]
pairedAntennas m =
  let matchingAntennas = filter (\(a, b) -> snd a == snd b) (pairs (antennas m))
   in map (\(a, b) -> (fst a, fst b, snd a)) matchingAntennas

antennaVec :: (Position, Position, Char) -> Vec
antennaVec (a, b, _) = vec a b

type Line = (Position, Position)

positions :: Map -> [Position]
positions m =
  [(i, j) | ((i, j), c) <- concatMap antinodes (pairedAntennas m), inRange (bounds m) (i, j), m ! (i, j) /= c]

antinodes :: (Position, Position, Char) -> [(Position, Char)]
antinodes (a, b, c) = [(add b dxdy, c), (add a (neg dxdy), c)]
  where
    dxdy = vec a b

positionsOnLines :: Map -> [Position]
positionsOnLines m =
  [ (i, j)
    | ((i, j), c) <- concatMap (\t -> line t (bounds m)) (pairedAntennas m),
      inRange (bounds m) (i, j)
  ]

line :: (Position, Position, Char) -> ((Int, Int), (Int, Int)) -> [(Position, Char)]
line (a, b, c) boun = posWithC ++ negWithC
  where
    negWithC = map (,c) negLine
    posWithC = map (,c) posLine
    negLine = takeWhile (inRange boun) (iterate (add (neg dxdy)) a)
    posLine = takeWhile (inRange boun) (iterate (add dxdy) b)
    dxdy = vec a b

draw :: Map -> String
draw m =
  let ((iMin, jMin), (iMax, jMax)) = bounds m
   in unlines [concat [[m ! (r, c)] | c <- [iMin .. iMax]] | r <- [jMin .. jMax]]

solve :: String -> Int
solve s =
  let m = parse s
   in length (nub (positions m))

solvePartTwo :: String -> Int
solvePartTwo s =
  let m = parse s
   in length (nub (positionsOnLines m))

place :: Map -> Position -> Char -> Map
place m p c = m // [(p, c)]

placeMany :: Map -> [Position] -> Char -> Map
placeMany m ps c = m // placements
  where
    placements = map (,c) ps

withAnti :: Map -> Map
withAnti m = placeMany m (positions m) '#'

withAntiLines :: Map -> Map
withAntiLines m = placeMany m (positionsOnLines m) '#'

main :: IO ()
main = do
  raw <- readInput
  print $ solvePartTwo raw
  print $ solve raw
