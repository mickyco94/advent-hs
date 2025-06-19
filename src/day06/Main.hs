module Main where

import Data.List (find)
import qualified Data.Set as Set
import System.Environment (getArgs)

data Direction = U | D | L | R deriving (Show, Eq)

data Tile = Dot | Hash | Guard Direction deriving (Show, Eq)

type Map = [[Tile]]

type Position = (Int, Int)

parse :: String -> Map
parse s = map (map charToTile) (lines s)

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
obstacles m = [(x, y) | (x, row) <- zip [0 ..] m, (y, cell) <- zip [0 ..] row, cell == Hash]

findInMap :: (Tile -> Bool) -> Map -> Maybe (Position, Tile)
findInMap p m =
  find (p . snd) indexed
  where
    indexed = [((x, y), val) | (x, row) <- zip [0 ..] m, (y, val) <- zip [0 ..] row]

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
      | outOfBounds pos = s
      | move pos dir `elem` obstacles m = go s pos (shiftDir dir)
      | otherwise = go (Set.insert pos s) (move pos dir) dir
      where
        outOfBounds (x, y) = x < 0 || x >= length m || y >= length (head m) || y < 0

-- I need distinct positions, so I should collect a set of tuples rather than
-- counting

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      print $ solve raw
    _ -> putStrLn "no input provided"
