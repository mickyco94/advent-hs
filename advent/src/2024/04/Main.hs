module Main where

import System.Environment (getArgs)

type Grid a = [[a]]

solve :: String -> Int
solve s = length (readXmas (lines s))

solvePartTwo :: String -> Int
solvePartTwo s = length (readCrossMas (lines s))

startingPoints :: Grid Char -> Char -> [(Int, Int)]
startingPoints grid c = [(x, y) | (x, row) <- zip [0 ..] grid, (y, val) <- zip [0 ..] row, val == c]

directions :: [(Int, Int)]
directions =
  [ (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0),
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1)
  ]

-- matchesWord finds the word in the direction given by (dy,dx) from (y,x)
matchesWord :: Grid Char -> (Int, Int) -> (Int, Int) -> String -> Bool
matchesWord grid (y, x) (dy, dx) = go y x
  where
    numRows = length grid
    numCols = if null grid then 0 else length (head grid)

    inBounds i j = i >= 0 && i < numRows && j >= 0 && j < numCols

    go _ _ [] = True
    go i j (c : cs)
      | not (inBounds i j) = False
      | grid !! i !! j /= c = False
      | otherwise = go (i + dy) (j + dx) cs

readXmas :: Grid Char -> [(Int, Int)]
readXmas grid = [pos | pos <- startingPoints grid 'X', dir <- directions, matchesWord grid pos dir "XMAS"]

hasCrossMas :: Grid Char -> (Int, Int) -> Bool
hasCrossMas grid (y, x)
  | y - 1 < 0 = False
  | x - 1 < 0 = False
  | y + 1 >= length grid = False
  | x + 1 >= length (head grid) = False
  | firstDiagonal && secondDiagonal = True
  | otherwise = False
  where
    topLeft c = grid !! (y - 1) !! (x - 1) == c
    topRight c = grid !! (y - 1) !! (x + 1) == c
    bottomLeft c = grid !! (y + 1) !! (x - 1) == c
    bottomRight c = grid !! (y + 1) !! (x + 1) == c
    firstDiagonal = (topLeft 'M' && bottomRight 'S') || (topLeft 'S' && bottomRight 'M')
    secondDiagonal = (bottomLeft 'M' && topRight 'S') || (bottomLeft 'S' && topRight 'M')

readCrossMas :: Grid Char -> [(Int, Int)]
readCrossMas grid = [pos | pos <- startingPoints grid 'A', hasCrossMas grid pos]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ solve contents
      print $ solvePartTwo contents
    _ -> print "Please provide an input file"
