module Main where

import Data.Array (Array, Ix (inRange), array, bounds, (!), (//))
import qualified Data.Array as Array
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import MyLib (Parser, input)
import Pos (Position, neighbours)
import Text.Megaparsec (oneOf, sepEndBy1, some)
import Text.Megaparsec.Char (eol)

type Maze = Array (Int, Int) Char

-- | parser interprets our input
parser :: Parser Maze
parser = do
  l <- some (oneOf "#.ES") `sepEndBy1` eol
  let entries = [((y, x), val) | (y, row) <- zip [0 ..] l, (x, val) <- zip [0 ..] row]
      numRows = length l
      numCols = length (head l)
  pure $ array ((0, 0), (numRows - 1, numCols - 1)) entries

-- | Finds the min path in a 2D array of 'S#.E' where # represents walls, S is the start and E is the end
minPath' :: Maze -> Maybe Int
minPath' g = go [(start g, 0)] Map.empty
  where
    go :: [(Position, Int)] -> Map.Map Position Int -> Maybe Int
    go [] m = Map.lookup (end g) m
    go ((p, steps) : ps) m
      | not (inRange (bounds g) p) = go ps m
      | g ! p == '#' = go ps m
      | otherwise = case Map.lookup p m of
          Just v | v <= steps -> go ps m
          _ -> go next m'
            where
              next = [(n, steps + 1) | n <- neighbours p] ++ ps
              m' = Map.insert p steps m

-- | cheats gives all the mazes with cheats applied
cheats :: Maze -> [Maze]
cheats base = [base // [(b, '.')] | b <- blocked base]

-- | blocked yields all the positions that are blocked
blocked :: Maze -> [Position]
blocked maze = [p | (p, '#') <- Array.assocs maze]

-- | start returns the start position of the maze
start :: Maze -> Position
start maze = head [pos | (pos, 'S') <- Array.assocs maze]

-- | end returns the end position of the maze
end :: Maze -> Position
end maze = head [pos | (pos, 'E') <- Array.assocs maze]

draw :: Maze -> String
draw m = unlines [concat [[m ! (r, c)] | c <- [iMin .. iMax]] | r <- [jMin .. jMax]]
  where
    ((iMin, jMin), (iMax, jMax)) = bounds m

main :: IO ()
main = do
  maze <- input 2024 20 parser
  print $ minPath' maze
  case minPath' maze of
    Nothing -> error "no way to reach the end"
    Just base ->
      print
        $ length
          . filter (>= 100)
          . map (base -)
          . mapMaybe minPath'
        $ cheats maze
