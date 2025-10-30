module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import MyLib (Parser, input)
import Text.Megaparsec
import Text.Megaparsec.Char (eol)
import Text.Printf (printf)
import Pos (Position, Dir, step, east, turnLeft, turnRight)

-- Walk represents the current position and direction
type Walk = (Int, Position, Dir, Set.Set Position)

type Maze = Map.Map Position Char

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)

-- | reads the input as a Map of Coordinates to Values
parser :: Parser Maze
parser = do
  l <- some (oneOf "#.ES") `sepEndBy1` eol
  let entries = [((y, x), val) | (y, row) <- zip [0 ..] l, (x, val) <- zip [0 ..] row]
  return (Map.fromList entries)

start :: Maze -> Position
start m = head [p | (p, 'S') <- Map.assocs m]

end :: Maze -> Position
end m = head [p | (p, 'E') <- Map.assocs m]

fill :: Maze -> Map.Map (Position, Dir) Int
fill m = go [(0, start m, east, Set.empty)] Map.empty
  where
    go :: [Walk] -> Map.Map (Position, Dir) Int -> Map.Map (Position, Dir) Int
    go [] counter = counter
    go ((score, p, d, seen) : ps) counter
      | Nothing <- Map.lookup p m = go ps counter
      | Just '#' <- Map.lookup p m = go ps counter
      | Just best <- Map.lookup (p, d) counter, best < score = go ps counter
      | otherwise = go (next ++ ps) (Map.insert (p, d) score counter)
      where
        straight = (score + 1, step p d, d, seen)
        left = (score + 1001, step p (turnLeft d), turnLeft d, seen)
        right = (score + 1001, step p (turnRight d), turnRight d, seen)
        next = [straight, left, right]


lookUpPos :: Position -> Map.Map (Position, Dir) Int -> Maybe Int
lookUpPos p m = safeMinimum [score | ((p', _), score) <- Map.toList m, p == p']

main :: IO ()
main = do
  maze <- input 2024 16 parser
  -- Part 2 not done
  print (lookUpPos (end maze) (fill maze))
