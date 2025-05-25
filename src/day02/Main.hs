module Main where

import System.Environment (getArgs)

withPrevious :: [a] -> [(a, a)]
withPrevious l = zip l (drop 1 l)

decreasing :: (Ord a) => [a] -> Bool
decreasing input = all (uncurry (>)) (withPrevious input)

increasing :: (Ord a) => [a] -> Bool
increasing input = all (uncurry (<)) (withPrevious input)

smallDiff :: (Num a, Ord a) => [a] -> Bool
smallDiff input = all (\(x, y) -> 0 < abs (x - y) && abs (x - y) < 4) (withPrevious input)

readInput :: String -> IO [[Int]]
readInput path = do
  contents <- readFile path
  return (map parseLine (lines contents))

parseLine :: String -> [Int]
parseLine = map read . words

safe :: (Num a, Ord a) => [a] -> Bool
safe l = smallDiff l && (decreasing l || increasing l)

removeEach :: [a] -> [[a]]
removeEach xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

tolerantSafe :: (Num a, Ord a) => [a] -> Bool
tolerantSafe l = any safe (removeEach l)

solve :: [[Int]] -> Int
solve = length . filter safe

solveTwo :: [[Int]] -> Int
solveTwo = length . filter tolerantSafe

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      reports <- readInput path
      print $ solve reports
      print $ solveTwo reports
    _ -> putStrLn "Please provide input file"
