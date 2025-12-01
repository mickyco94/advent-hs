module Main where

import Data.Array
import MyLib (Parser, input, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char

type Position = (Int, Int)

type Grid = Array Position Char

type Moves = [Char]

showGrid :: Grid -> String
showGrid g =
  let ((r0, c0), (r1, c1)) = bounds g
   in unlines [[g ! (r, c) | c <- [c0 .. c1]] | r <- [r0 .. r1]]

grid :: Parser Grid
grid = do
  rows <- filter (not . null) <$> rowChars `sepBy1` newline
  let numCols = if null rows then 0 else length (head rows)
      numRows = length rows
      b = ((0, 0), (numRows - 1, numCols - 1))
      pairs =
        [ ((r, c), v)
          | (r, row) <- zip [0 ..] rows,
            (c, v) <- zip [0 ..] row
        ]
  return $ array b pairs
  where
    rowChars =
      many $
        char '.'
          <|> char '#'
          <|> char 'O'
          <|> char '@'

moves :: Parser Moves
moves = many $ lexeme rowChars
  where
    rowChars =
      char '<'
        <|> char '>'
        <|> char '^'
        <|> char 'v'

parser :: Parser (Grid, Moves)
parser = do
  g <- grid
  m <- moves
  return (g, m)
-- | move applies the move to a Grid, returning an updated grid
move :: Grid -> Char -> Grid
move g d
  | g ! next start == '#' = g
  | blocked && null boxes = g
  | otherwise = g // cleared  // robotMoves // boxes
  where
    blocked = g ! next start `elem` "#[]O"
    start = robot g
    next p = step p d
    robotMoves = [(start, '.'), (next start, '@')]
    affected = pushed' g (next start) d
    cleared = [(p, '.') | p <- affected]
    boxes = [(next p, v) | p <- affected, let v = g ! p]

-- | step updates the position based on the direction
step :: Position -> Char -> Position
step (y, x) '<' = (y, x - 1)
step (y, x) '>' = (y, x + 1)
step (y, x) 'v' = (y + 1, x)
step (y, x) '^' = (y - 1, x)
step (y, x) _ = (y, x)

-- pushed gets the boxes in the grid impacted by a move
-- returning an empty list if it cannot be moved
-- Keep going as long as we have a 0, if at the end we reach a .
-- return the list, otherwise return []
pushed :: Grid -> Position -> Char -> [Position]
pushed g start d = go start []
  where
    go :: Position -> [Position] -> [Position]
    go p@(y, x) acc
      | not (inRange (bounds g) p) = acc
      | p `elem` acc = acc
      | g ! p == '.' = acc
      | g ! p == '#' = []
      | g ! p == '@' = go (step p d) acc
      | g ! p == 'O' = go (step p d) (p : acc)
      | g ! p == '[' = go (step p d) (p : acc) ++ go (y, x + 1) (p : acc)
      | g ! p == ']' = go (step p d) (p : acc) ++ go (y, x - 1) (p : acc)
      | otherwise = error $ "Unexpected grid char" ++ show (g ! p)

pushed' :: Grid -> Position -> Char -> [Position]
pushed' g start d = go [start] []
  where
    go [] [] = []
    go [] acc = acc -- Search done
    go (p@(y, x) : xs) acc
      | not (inRange (bounds g) p) = go xs acc
      | p `elem` acc = go xs acc
      | g ! p == '#' = []
      | g ! p == 'O' = go (step p d : xs) (p : acc) -- Add the current
      | g ! p == '[' = go ([(y, x + 1), step p d] ++ xs) (p : acc) -- Add the neighbour
      | g ! p == ']' = go ([(y, x - 1), step p d] ++ xs) (p : acc)
      | otherwise = go xs acc -- Keep searching

robot :: Grid -> Position
robot g = head [p | (p, c) <- assocs g, c == '@']

gps :: Grid -> Int
gps g = sum [100 * y + x | ((y, x), v) <- assocs g, v `elem` "[O"]

enlarge :: Grid -> Grid
enlarge g =
  let ((sy, sx), (ey, ex)) = bounds g
      rows = [[g ! (y, x) | x <- [sx .. ex]] | y <- [sy .. ey]]
      enlargedRows = map (concatMap enlargeCell) rows
      newEx = sx + length (head enlargedRows) - 1
   in listArray ((sy, sx), (ey, newEx)) (concat enlargedRows)

enlargeCell :: Char -> String
enlargeCell 'O' = "[]"
enlargeCell '@' = "@."
enlargeCell v = [v, v]

main :: IO ()
main = do
  (g, m) <- input 2024 15 parser
  print (gps (foldl move g m))
  print (gps (foldl move (enlarge g) m))
