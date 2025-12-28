module BitBoard where

import Control.Applicative (asum)
import Data.Bits
import Data.Char (intToDigit)
import Data.List (foldl', foldl1', intercalate, nub, sortOn)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import Numeric (showIntAtBase)
import Pos (Position, mh, neighbours)
import Text.Printf (printf)

data BitBoard
  = Empty !Int !Int
  | B
      !Int -- Height
      !Int -- Width
      (V.Vector Word64)
  | Union [BitBoard]
  deriving (Eq, Ord)

instance Show BitBoard where
  show (B h w board) =
    "B: "
      ++ show h
      ++ "x"
      ++ show w
      ++ " "
      ++ intercalate "x" (map fmt (V.toList board))
   where
    fmt = printf ("%0" ++ show w ++ "b")

height :: BitBoard -> Int
height (B h _ _) = h

width :: BitBoard -> Int
width (B _ w _) = w

board :: BitBoard -> V.Vector Word64
board (B _ _ b) = b

empty :: Int -> Int -> BitBoard
empty = Empty

singleton :: Int -> Int -> (Int, Int) -> BitBoard
singleton h w (y, x) = fromList h w [(y, x)]

fromList :: Int -> Int -> [(Int, Int)] -> BitBoard
fromList h w xs = setMany xs (empty h w)

null :: BitBoard -> Bool
null (B _ _ b) = V.all (== 0) b

mask :: BitBoard -> Int
mask (B h w _) = bit (h * w) - 1

set :: Int -> Int -> BitBoard -> BitBoard
set y x (B h w b)
  | y < 0 || y >= h = B h w b
  | x < 0 || x >= w = B h w b
  | otherwise = B h w (b V.// update)
 where
  existing = b V.! y
  update = [(y, existing .|. shiftL 1 x)]

setMany :: [(Int, Int)] -> BitBoard -> BitBoard
setMany xs b = foldl step b xs
 where
  step acc (y, x) = set y x acc

has :: Int -> Int -> BitBoard -> Bool
has y x (B h w b)
  | y < 0 || x < 0 = False
  | y >= V.length b = False
  | otherwise = shiftL 1 x .&. row /= 0
 where
  row = b V.! y

-- Displays the board as a two-dimensional grid, for printing.
render :: BitBoard -> String
render bb@(B h w b) =
  unlines
    [ [ if has y x bb then '1' else '0'
      | x <- [0 .. w - 1]
      ]
    | y <- [0 .. h - 1]
    ]

translate :: Int -> Int -> BitBoard -> BitBoard
translate dy dx bb@(B h w b) = B h w b'
 where
  dx' = abs dx
  dy' = abs dy

  rowMask = bit w - 1

  row r
    | dx >= 0 = r `shiftL` dx' .&. rowMask
    | dx < 0 = r `shiftR` dx' .&. rowMask

  b'
    | dy >= 0 = V.replicate dy' 0 V.++ V.take (h - dy') (V.map row b)
    | dy < 0 = V.drop dy' (V.map row b) V.++ V.replicate dy' 0

rotateCoords :: (Int, Int) -> Int -> Int -> (Int, Int)
rotateCoords (y, x) h w = (x, h - y - 1)

rotateBoard :: BitBoard -> BitBoard
rotateBoard board@(B h w b) = fromList h w rotatedBits
 where
  rotatedBits =
    [ rotateCoords (y, x) h w
    | y <- [0 .. h - 1]
    , x <- [0 .. w - 1]
    , has y x board
    ]

rotations :: BitBoard -> [BitBoard]
rotations b = [b' | b' <- take 4 (iterate rotateBoard b), size b' == size b]

adjacent :: BitBoard -> BitBoard -> Bool
adjacent a b =
  or
    [ has y' x' b
    | (y, x) <- populated a
    , (y', x') <- neighbours (y, x)
    , y' >= 0
    , x' >= 0
    ]

populated :: BitBoard -> [Position]
populated b = [(y, x) | y <- [0 .. height b - 1], x <- [0 .. width b - 1], has y x b]

resize :: Int -> Int -> BitBoard -> BitBoard
resize h w (B oh ow b) = B h w b'
 where
  b'
    | h <= oh = V.take h (V.map row b)
    | h > oh = V.map row b V.++ V.replicate (h - oh) 0

  rmask = bit w - 1

  row r
    | w >= ow = r
    | w < ow = r .&. rmask

union :: BitBoard -> BitBoard -> BitBoard
union (B h1 w1 b1) (B h2 w2 b2)
  | h1 == h2 && w1 == w2 = B h1 w1 (V.zipWith (.|.) b1 b2)
  | otherwise = error "union of boards must be of the same size"

disjoint :: BitBoard -> BitBoard -> Bool
disjoint (B h1 w1 b1) (B h2 w2 b2)
  | h1 == h2 && w1 == w2 = V.all (\(a, b) -> a .&. b == 0) (V.zip b1 b2)
  | otherwise = error "Mismatched sizes, panic"

size :: BitBoard -> Int
size (B _ _ b) = V.sum (V.map popCount b)

remainingSpace :: BitBoard -> Int
remainingSpace bb@(B h w b) = (h * w) - size bb

placements :: BitBoard -> BitBoard -> [BitBoard]
placements a b = [a' | a' <- concatMap arrangements resized, disjoint a' b]
 where
  resize' = resize (height b) (width b)
  resized = map resize' (a : rotations a)

place :: BitBoard -> BitBoard -> [BitBoard]
place a b = nub [normalise (a' `union` b) | a' <- placements a b]

place' :: BitBoard -> BitBoard -> [BitBoard]
place' a b = concat [placeNext' a' b | a' <- rotations a]

placeNext' :: BitBoard -> BitBoard -> [BitBoard]
placeNext' a b =
  [ newA `union` b
  | (y, x) <- growFrom (0, 0) (height b, width b)
  , let newA = translate y x resized
  , newA `disjoint` b
  , size newA == size a
  , newA `adjacent` b || has 0 0 newA
  ]
 where
  resized = resize (height b) (width b) a

-- Returns the logical next placement of a in b
placeNext :: BitBoard -> BitBoard -> Maybe BitBoard
placeNext a b =
  listToMaybe
    [ newA `union` b
    | (y, x) <- growFrom (0, 0) (height b, width b)
    , let newA = translate y x resized -- Translate allows moving out of bounds!
    , newA `disjoint` b
    , size newA == size a
    ]
 where
  resized = resize (height b) (width b) a

normalise :: BitBoard -> BitBoard
normalise b
  | size b == 0 = b
  | not hasLeft = normalise (translate 0 (-1) b)
  | not hasTop = normalise (translate (-1) 0 b)
  | otherwise = b
 where
  rowMask = bit (width b) - 1
  hasLeft = V.any (\r -> (r .&. 1) /= 0) (board b)
  hasTop = (V.head (board b) .&. rowMask) /= 0

arrangements :: BitBoard -> [BitBoard]
arrangements bb@(B h w b) =
  [ bb'
  | (y, x) <- growFrom (-h, -w) (h - 1, w - 1)
  , let bb' = translate y x bb
  , size bb' == size bb
  ]

growFrom :: Position -> Position -> [Position]
growFrom (y1, x1) (y2, x2) =
  sortOn
    (mh (y1, x1))
    [ (y', x')
    | y' <- [y1 .. y2]
    , x' <- [x1 .. x2]
    ]
