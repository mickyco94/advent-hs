module Main where

import Data.List (find, inits, mapAccumL, sortBy, sortOn)
import Data.Ord (Down (Down), comparing)
import qualified Data.Set as Set
import MyLib (Parser, input, integer)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Prelude

-- (x,y,z)
type Coord = (Int, Int, Int)

parser :: Parser [Coord]
parser = coord `sepEndBy1` newline
  where
    coord = do
      x <- integer <* char ','
      y <- integer <* char ','
      z <- integer
      pure (x, y, z)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

cartProdSelf :: [a] -> [(a, a)]
cartProdSelf xs = [(xs !! i, x') | i <- [0 .. n], x' <- drop (i + 1) xs]
  where
    n = length xs - 1

cartDist :: Coord -> Coord -> Float
cartDist (x, y, z) (x', y', z') = (sqrt . fromIntegral) (sum [dist x x', dist y y', dist z z'])
  where
    dist a b = (b - a) ^ 2

connect :: [Set.Set Coord] -> (Coord, Coord) -> [Set.Set Coord]
connect ss (a, b) = combine (Set.fromList [a, b] : ss)

-- | merge combines the set with others it overlaps with
merge :: (Ord a) => [Set.Set a] -> Set.Set a -> [Set.Set a]
merge ss s
  | null overlap = s : ss
  | otherwise = rest ++ map (`Set.union` s) overlap
  where
    hasIntersection s' = Set.size (s `Set.intersection` s') > 0
    overlap = filter hasIntersection ss
    rest = filter (not . hasIntersection) ss

-- | merges all the sets together if they overlap
combine :: (Ord a) => [Set.Set a] -> [Set.Set a]
combine = foldl merge []

flatSet :: (Ord a) => [Set.Set a] -> Set.Set a
flatSet = foldl Set.union Set.empty

xproduct :: Coord -> Coord -> Int
xproduct (x, _, _) (x', _, _) = x * x'

foldUntil :: (acc -> x -> acc) -> (acc -> Bool) -> acc -> [x] -> Maybe x
foldUntil _ _ _ [] = Nothing
foldUntil f p acc (x : xs)
  | p acc' = Just x
  | otherwise = foldUntil f p acc' xs
  where
    acc' = f acc x

main :: IO ()
main = do
  coords <- input 2025 8 parser
  let n = 1000
      prod = cartProdSelf coords
      sorted = sortOn (uncurry cartDist) prod
      circuits = foldl connect [] (take n sorted)
      biggest = take 3 (sortBy (comparing Data.Ord.Down) (map Set.size circuits))
      done ss = length ss == 1 && Set.size (flatSet ss) == length coords
      connectingPair' = foldUntil connect done [] sorted

  print (product biggest)
  print (uncurry xproduct <$> connectingPair')
