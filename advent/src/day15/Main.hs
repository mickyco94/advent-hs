module Main where

import Data.Array
import MyLib (Parser, lexeme, input)
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
  | g ! next start == 'O' && null boxes = g
  | otherwise = g // updates
  where
    start = robot g
    next p = step p d
    robotMoves = [(start, '.'), (next start, '@')]
    boxes = [(next p, 'O') | p <- pushed g start d]
    updates = boxes ++ robotMoves

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
    go p acc
      | not (inRange (bounds g) p) = acc
      | g ! p == '@' = go (step p d) acc
      | g ! p == '.' = acc
      | g ! p == 'O' = go (step p d) (p : acc)
      | g ! p == '#' = []
      | otherwise = error $ "Unexpected grid char" ++ show (g ! p)

robot :: Grid -> Position
robot g = head [p | (p, c) <- assocs g, c == '@']

gps :: Grid -> Int
gps g = sum [100 * y + x | ((y, x), 'O') <- assocs g]

main :: IO ()
main = do
  (g, m) <- input 2024 15 parser
  print (gps (foldl move g m))
