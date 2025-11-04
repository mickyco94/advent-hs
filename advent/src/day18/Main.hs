{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Tuple (swap)
import MyLib (Parser, input, tuple)
import Pos (Position, neighbours)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (newline)

gridSize :: Int
gridSize = 70

start :: Position
start = (0, 0)

end :: Position
end = (gridSize, gridSize)

inBounds :: Position -> Bool
inBounds (y, x) =
  y >= 0
    && y <= gridSize
    && x >= 0
    && x <= gridSize

-- N.B. The input positions are in form (x,y) when we typically use (y,x)
parser :: Parser [Position]
parser = do
  raw <- tuple `sepEndBy1` newline
  pure $ map swap raw

minPath :: [Position] -> Maybe Int
minPath bytes = go [(start, 0)] Map.empty
  where
    go :: [(Position, Int)] -> Map.Map Position Int -> Maybe Int
    go [] counter = Map.lookup end counter
    go ((p, steps) : ps) counter
      | p `elem` bytes = go ps counter
      | not (inBounds p) = go ps counter
      | Just best <- Map.lookup p counter, best <= steps = go ps counter
      | otherwise = go (ps ++ next) (Map.insert p steps counter)
      where
        next = map (,steps + 1) (neighbours p)

firstBlock :: [Position] -> Maybe Position
firstBlock ps = safeHead paths
  where 
    pow = [ (take n ps, ps !! (n-1)) | n <- [1..length ps]]
    paths = [p | (bytes, p) <- pow, isNothing (minPath bytes) ]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  positions <- input 2024 18 parser
  print (minPath (take 1024 positions))
  print (swap <$> firstBlock positions) 
