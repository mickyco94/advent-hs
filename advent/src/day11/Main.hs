module Main where

import Data.Map (Map)
import qualified Data.Map as Map

type Stone = Int

blinks :: Map Stone Int -> Map Stone Int
blinks m = Map.fromListWith (+) [(stone, n) | (s, n) <- Map.assocs m, stone <- blink s]

blink :: Stone -> [Stone]
blink 0 = [1]
blink s
  | even (numDigits s) = [left, right]
  | otherwise = [s * 2024]
  where
    (left, right) = split s

split :: Int -> (Int, Int)
split n = (read left, read right)
  where
    mid = numDigits n `div` 2
    (left, right) = splitAt mid (show n)

numDigits :: Int -> Int
numDigits n = length (show n)

parse :: String -> [Stone]
parse s = map read (words s)

asMap :: [Stone] -> Map Stone Int
asMap ss = Map.fromListWith (+) [(stone, 1) | stone <- ss]

solve :: Int -> [Stone] -> Int
solve n ss = sum (iterate blinks (asMap ss) !! n)

input :: [Stone]
input = parse "5 127 680267 39260 0 26 3553 5851995"

main :: IO ()
main = print input
