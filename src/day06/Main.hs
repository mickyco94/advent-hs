module Main where

import Data.Array
import Data.Foldable (find)
import qualified Data.Set as Set
import System.Environment (getArgs)

data Direction = U | D | L | R deriving (Show, Eq, Ord)

data Tile = Dot | Hash | Guard Direction deriving (Show, Eq)

type Map = Array (Int, Int) Tile

type Position = (Int, Int)

parse :: String -> Map
parse s = array b entries
  where
    rows = lines s
    numRows = length rows
    numCols = length (head rows)
    b = ((0, 0), (numRows - 1, numCols - 1))
    entries = [((i, j), charToTile cell) | (i, row) <- zip [0 ..] rows, (j, cell) <- zip [0 ..] row]

charToTile :: Char -> Tile
charToTile '.' = Dot
charToTile '#' = Hash
charToTile '^' = Guard U
charToTile '>' = Guard R
charToTile '<' = Guard L
charToTile 'v' = Guard D
charToTile _ = error "invalid char"

isGuard :: Tile -> Bool
isGuard (Guard _) = True
isGuard _ = False

obstacles :: Map -> [Position]
obstacles m = [(x, y) | ((x, y), cell) <- assocs m, cell == Hash]

findInMap :: (Tile -> Bool) -> Map -> Maybe (Position, Tile)
findInMap p m = find (p . snd) (assocs m)

solve :: String -> Maybe Int
solve s = do
  let m = parse s
  (start, Guard dir) <- findInMap isGuard m
  let c = walk m start dir
  let size = Set.size c
  return size

shiftDir :: Direction -> Direction
shiftDir U = R
shiftDir R = D
shiftDir D = L
shiftDir L = U

move :: Position -> Direction -> Position
move (y, x) U = (y - 1, x)
move (y, x) L = (y, x - 1)
move (y, x) R = (y, x + 1)
move (y, x) D = (y + 1, x)

walk :: Map -> Position -> Direction -> Set.Set Position
walk m = go Set.empty
  where
    go :: Set.Set Position -> Position -> Direction -> Set.Set Position
    go s pos dir
      | not (inRange (bounds m) pos) = s
      | move pos dir `elem` obstacles m = go s pos (shiftDir dir)
      | otherwise = go (Set.insert pos s) (move pos dir) dir

isLoop :: Map -> Position -> Direction -> Bool
isLoop m start startD = go start startD Set.empty
  where
    go pos dir visited
      | Set.member (pos, dir) visited = True
      | not (inRange (bounds m) pos) = False
      | move pos dir `elem` obstacles m = go pos (shiftDir dir) (Set.insert (pos, dir) visited)
      | otherwise = go (move pos dir) dir (Set.insert (pos, dir) visited)

solvePartTwo :: String -> Maybe Int
solvePartTwo s = do
  let m = parse s
  (start, Guard dir) <- findInMap isGuard m
  let candidates = Set.delete start (walk m start dir)
  let options = [pos | pos <- Set.toList candidates, isLoop (m // [(pos, Hash)]) start dir]
  return (length options)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      print $ solve raw
      print $ solvePartTwo raw
    _ -> putStrLn "no input provided"
