module Main where

import Data.List (partition, sortOn, tails)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Ord
import MyLib (Parser, input, integer)
import Pos (Position)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline)
import Prelude

-- | Rect defines a rectangle in 2D space
data Rect
  = Rect
      Range -- [y0,y1)
      Range -- [x0,x1)
  deriving (Show, Eq)

-- Range is an inclusive, exclusive interval
type Range = (Int, Int)

parser :: Parser [Position]
parser = pos `sepEndBy1` newline
  where
    pos = do
      x <- integer <* char ','
      y <- integer
      pure (y, x)

range :: Int -> Int -> Range
range a b = (min a b, max a b)

gap :: Range -> Range -> Range
gap (a, b) (c, d)
  | maxAB < minCD = (maxAB, minCD)
  | maxCD < minAB = (maxCD, minAB)
  | otherwise = (0, 0)
  where
    maxAB = max a b
    minAB = min a b
    minCD = min c d
    maxCD = max c d

rect :: Range -> Range -> Maybe Rect
rect (y0, y1) (x0, x1)
  | y0 >= y1 = Nothing
  | x0 >= x1 = Nothing
  | otherwise = Just (Rect y x)
  where
    y = range y0 y1
    x = range x0 x1

cartProd :: [a] -> [(a, a)]
cartProd xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

fromDiagonal :: Position -> Position -> Maybe Rect
fromDiagonal (y, x) (y', x') = rect (y, y') (x, x')

intersectRect :: Rect -> Rect -> Maybe Rect
intersectRect (Rect (ay1, ay2) (ax1, ax2)) (Rect (by1, by2) (bx1, bx2))
  | x1 > x2 || y1 > y2 = Nothing
  | otherwise = rect (y1, y2) (x1, x2)
  where
    x1 = max ax1 bx1
    x2 = min ax2 bx2
    y1 = max ay1 by1
    y2 = min ay2 by2

subtractRects :: [Rect] -> [Rect] -> [Rect]
subtractRects = foldl (\acc r -> concatMap (`subtractRect` r) acc)

subtractRect :: Rect -> Rect -> [Rect]
subtractRect a@(Rect (ay0, ay1) (ax0, ax1)) b = case intersectRect a b of
  Nothing -> [a]
  Just (Rect (iy0, iy1) (ix0, ix1)) -> catMaybes [below, above, right, left]
    where
      left = rect (ay0, ay1) (ax0, ix0)
      right = rect (iy0, iy1) (ix1, ax1)
      below = rect (ay0, iy0) (ix0, ax1)
      above = rect (iy1, ay1) (ix0, ax1)

area :: Rect -> Int
area (Rect (y1, y2) (x1, x2)) = w * h
  where
    w = x2 - x1
    h = y2 - y1

maximal :: [Position] -> Rect
maximal ps = Rect (minY - 1, maxY + 1) (minX - 1, maxX + 1)
  where
    minY = minimum (map fst ps)
    maxY = maximum (map fst ps) + 1
    minX = minimum (map snd ps)
    maxX = maximum (map snd ps) + 1

connected :: Rect -> Rect -> Bool
connected (Rect ay ax) (Rect by bx) =
  gap ay by == (0, 0) && gap ax bx == (0, 0)

connectedRects :: [Rect] -> [Rect] -> [Rect]
connectedRects [] _ = []
connectedRects (r : rs) source = r : connectedRects (connecting ++ rs) avail
  where
    (connecting, avail) = partition (connected r) source

toRect :: Position -> Position -> Maybe Rect
toRect (ay, ax) (by, bx) = rect (min ay by, max ay by + 1) (min ax bx, max ax bx + 1)

main :: IO ()
main = do
  coords <- input 2025 9 parser

  let border = [r | (a : b : _) <- tails (coords ++ [head coords]), Just r <- [toRect a b]]
      rects = sortOn (Data.Ord.Down . area) [r | (a, b) <- cartProd coords, Just r <- [toRect a b]]

      inverse = subtractRects [maximal coords] border
      outside = connectedRects [head inverse] inverse
      outOfBounds r = any (isJust . intersectRect r) outside

      validRects = filter (not . outOfBounds) rects
      best = head validRects

  print (area (head rects))
  print (area best)
