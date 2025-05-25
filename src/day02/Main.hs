module Main where

import System.Environment (getArgs)

withPrevious :: [a] -> [(a, a)]
withPrevious l = zip l (drop 1 l)

decreasing :: (Ord a) => [a] -> Bool
decreasing input = all (uncurry (>)) (withPrevious input)

increasing :: (Ord a) => [a] -> Bool
increasing input = all (uncurry (<)) (withPrevious input)

safe :: (Num a, Ord a) => [a] -> Bool
safe input = all (\(x, y) -> 0 < abs (x - y) && abs (x - y) < 4) (withPrevious input)

readInput :: String -> IO [[Int]]
readInput path = do
  contents <- readFile path
  return (map parseLine (lines contents))

parseLine :: String -> [Int]
parseLine = map read . words

valid :: (Num a, Ord a) => [a] -> Bool
valid l = safe l && (decreasing l || increasing l)

solve :: [[Int]] -> Int
solve = length . filter valid

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      reports <- readInput path
      print $ solve reports
    _ -> putStrLn "Please provide input file"
