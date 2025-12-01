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
  let c = map fst $ walk m start dir
  return (length (dedup c))

dedup :: (Ord a) => [a] -> [a]
dedup xs = Set.toList (Set.fromList xs)

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

walk :: Map -> Position -> Direction -> [(Position, Direction)]
walk m = go []
  where
    go :: [(Position, Direction)] -> Position -> Direction -> [(Position, Direction)]
    go s pos dir
      | not (inRange (bounds m) pos) = s
      | not (inRange (bounds m) next) = s
      | next `elem` obstacles m = go ((pos, shiftDir dir) : s) pos (shiftDir dir) -- Next is blocked, so keep turning right
      | otherwise = go ((next, dir) : s) (move pos dir) dir
      where
        next = move pos dir

isLoop :: (Eq a) => [a] -> Bool
isLoop a = go a a
  where
    go (x : xs) (_ : y : ys) = x == y || go xs ys
    go _ _ = False

solvePartTwo :: String -> Maybe Int
solvePartTwo s = do
  let m = parse s
  (start, Guard dir) <- findInMap isGuard m
  let c = map fst $ walk m start dir
  let options = [pos | pos <- c, isLoop $ walk (m // [(pos, Hash)]) start dir]
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
