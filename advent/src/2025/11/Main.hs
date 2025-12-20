module Main where

import qualified Data.Map as Map
import MyLib (Parser, input)
import Text.Megaparsec (manyTill, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (char, letterChar, newline)
import Prelude

parser :: Parser (Map.Map String [String])
parser = Map.fromList <$> line `sepEndBy1` newline
  where
    line :: Parser (String, [String])
    line = do
      key <- manyTill letterChar (char ':') <* char ' '
      values <- some letterChar `sepBy1` char ' '
      pure (key, values)

inDeg :: (Ord a) => Map.Map a [a] -> [(a, Int)]
inDeg m = [(k, inDegreeOfNode k) | k <- Map.keys m]
  where
    inDegreeOfNode k = length (concatMap (filter (== k)) (Map.elems m))

topo :: (Ord a) => Map.Map a [a] -> [a]
topo m
  | null m = []
  | otherwise = x : topo m'
  where
    x = head [k | (k, 0) <- inDeg m]
    m' = Map.fromList [(k, filter (/= x) v) | (k, v) <- Map.assocs m, k /= x]

parents :: String -> Map.Map String [String] -> [String]
parents x graph = [k | (k, v) <- Map.assocs graph, x `elem` v]

numWays :: String -> Map.Map String [String] -> Map.Map String Int
numWays start graph = foldl step initial (topo graph)
  where
    initial = Map.singleton start 1
    step acc x = Map.insertWith (+) x v acc
      where
        v = sum [acc Map.! p | p <- parents x graph]

paths :: String -> String -> Map.Map String [String] -> Int
paths from to graph = numWays from graph Map.! to

paths' :: [String] -> Map.Map String [String] -> Int
paths' [] _ = 1 -- from nowhere to nowhere
paths' [_] _ = 1 -- from here to here
paths' (x : y : xs) graph = paths x y graph * paths' (y : xs) graph

main :: IO ()
main = do
  rack <- input 2025 11 parser

  let rack' = Map.insert "out" [] rack

  print (paths' ["you", "out"] rack')
  print (paths' ["svr", "fft", "dac", "out"] rack')
