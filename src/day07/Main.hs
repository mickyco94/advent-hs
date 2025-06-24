module Main where

import Data.List (stripPrefix)
import System.Environment (getArgs)
import Text.Read (readMaybe)

readInput :: IO String
readInput = do
  args <- getArgs
  case args of
    [path] -> do
      readFile path
    _ -> error "no file"

type Input = (Int, [Int])

parse :: String -> Maybe [Input]
parse s = mapM parseLine (lines s)

parseLine :: String -> Maybe Input
parseLine s = do
  let targetS = takeWhile (/= ':') s
      targetTest = read targetS
  rest <- stripPrefix (targetS ++ [':']) s
  let numbersS = words rest
  numbers <- mapM readMaybe numbersS
  Just (targetTest, numbers)

solve :: [Input] -> Int
solve input =
  let options = map fst (filter canSolve input)
   in sum options

combos :: [Int] -> [Int]
combos = go []
  where
    go :: [Int] -> [Int] -> [Int]
    go [] (x : xs) = go [x] xs
    go acc [] = acc
    go acc (x : xs) = go (sums ++ multis ++ concats) xs
      where
        sums = map (+ x) acc
        multis = map (* x) acc
        concats = map (`concatInts` x) acc

concatInts :: Int -> Int -> Int
concatInts x y = read (show x ++ show y)

canSolve :: Input -> Bool
canSolve (target, nums) = target `elem` combos nums

main :: IO ()
main = do
  raw <- readInput
  case parse raw of
    Just input -> print $ solve input
    Nothing -> putStrLn "invalid input"
