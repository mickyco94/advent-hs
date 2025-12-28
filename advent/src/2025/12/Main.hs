module Main where

import BitBoard (BitBoard, empty, fromList, place')
import Data.Char (digitToInt)
import Data.Foldable (asum)
import Data.List (sortBy)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import MyLib (Parser, input, integer)
import Pos (Position)
import Text.Megaparsec (
  MonadParsec (try),
  oneOf,
  sepEndBy1,
  some,
 )
import Text.Megaparsec.Char (
  char,
  digitChar,
  eol,
  hspace,
  newline,
  space,
 )

data Shape = Shape Int (Grid Char) deriving (Show, Eq)

type Grid a = [[a]]
type Grid' = Set.Set Position

data Region
  = Region
      Int -- Width
      Int -- Height
      [Int]
  deriving (Show, Eq)

instance Ord Shape where
  compare (Shape _ cells1) (Shape _ cells2) = compare (filled cells1) (filled cells2)
   where
    filled = concatMap (filter (== '#'))

shapeParser :: Parser Shape
shapeParser = do
  index <- digitToInt <$> digitChar <* char ':' <* space
  rows <- some (oneOf ".#") `sepEndBy1` newline
  pure (Shape index rows)

regionParser :: Parser Region
regionParser = do
  w <- integer <* char 'x'
  h <- integer <* char ':' <* hspace
  indexes <- integer `sepEndBy1` hspace
  pure (Region w h indexes)

parser :: Parser ([Shape], [Region])
parser = do
  shapes <- try shapeParser `sepEndBy1` (hspace *> eol)
  regions <- regionParser `sepEndBy1` newline
  pure (shapes, regions)

shapeToBoard :: Shape -> BitBoard
shapeToBoard (Shape _ shape) = fromList h w cells
 where
  cells = [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1], shape !! y !! x == '#']
  h = length shape
  w = length (head shape)

apply :: [BitBoard] -> BitBoard -> Maybe BitBoard
apply [] b = Just b
apply (shape : xs) b = asum [apply xs b' | b' <- place' shape b]

canSolve :: Region -> [Shape] -> Bool
canSolve (Region w h amounts) shapes = isJust (apply shapeBoards (BitBoard.empty h w))
 where
  shapeBoards =
    sortBy
      (flip compare)
      [ shapeToBoard shape
      | (amt, shape) <- zip amounts shapes
      , amt > 0
      , _ <- [1 .. amt]
      ]

canNaive :: Region -> [Shape] -> Bool
canNaive (Region w h amounts) shapes = h * w >= neededSpace
 where
  size (Shape _ rows) = sum [1 | '#' <- concat rows]
  neededSpace = sum [size shape * amount | (shape, amount) <- zip shapes amounts]

main :: IO ()
main = do
  (shapes, regions) <- input 2025 12 parser

  print (length [r | r <- regions, canNaive r shapes])
