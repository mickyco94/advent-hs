module Main where

import System.Environment (getArgs)

type Grid a = [[a]]

solve :: String -> Int
solve s = length (readXmas (lines s))

startingPoints :: Grid Char -> [(Int, Int)]
startingPoints grid = [(x, y) | (x, row) <- zip [0 ..] grid, (y, val) <- zip [0 ..] row, val == 'X']

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
readXmas grid = [pos | pos <- startingPoints grid, dir <- directions, matchesWord grid pos dir "XMAS"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ solve contents
    _ -> print "Please provide an input file"
