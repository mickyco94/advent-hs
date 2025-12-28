module Grid where

import qualified Data.Set as Set

-- We can use bitmasking over a set of [Int]. Maybe a Vector of Int..?

data Grid
  = G
      Int -- Height
      Int -- Width
      (Set.Set (Int, Int)) -- Occupied coordinates
  deriving (Show)

initGrid :: Int -> Int -> Grid
initGrid h w = G h w Set.empty

insert :: Int -> Int -> Grid -> Grid
insert y x grid@(G h w cells)
  | y > h || y < 0 = grid
  | x > w || x < 0 = grid
  | otherwise = G h w (Set.insert (y, x) cells)

{- |
Updates the given coordinate to be blank, this is a no-op for out of bounds or already blank

1 0 x..    x..
    x.. -> ...
    ...    ...
-}
remove :: Int -> Int -> Grid -> Grid
remove y x grid@(G h w cells)
  | y > h || y < 0 = grid
  | x > w || x < 0 = grid
  | otherwise = G h w (Set.delete (y, x) cells)

{- |
Constructs a merge of two disjoint equally sized grids

xxx ...    xxx
x.. ... -> x..
... .xx    .xx
-}
overlay :: Grid -> Grid -> Maybe Grid
overlay (G y1 x1 cells1) (G y2 x2 cells2)
  | y1 /= y2 = Nothing
  | x1 /= x2 = Nothing
  | not (Set.null (cells1 `Set.intersection` cells2)) = Nothing
  | otherwise = Just (G y1 x1 (cells1 `Set.union` cells2))

{- |
Rotate applies a 90 degree clockwise rotation to the grid about the center of the grid.

x..    ..x
..x -> ...
...    .x.
-}
rotate :: Grid -> Grid
rotate grid@(G h w cells) = G w h (Set.fromList [(x, h - 1 - y) | (y, x) <- Set.toList cells])

-- Performs a mirror flip about the y-axis
hflip :: Grid -> Grid
hflip g = g

vflip :: Grid -> Grid
vflip g = g

{- |
Increases the size of the grid, maintaining its elements and positions
-}
grow :: Int -> Int -> Grid -> Grid
grow dy dx (G h w cells) = G (h + dy) (w + dx) cells

-- | Shifts all members of the grid by dy,dx. Dropping those that move out of bounds
shift :: Int -> Int -> Grid -> Grid
shift dy dx (G h w cells) = G h w (Set.fromList cells')
 where
  cells' =
    [ (y + dy, x + dx)
    | (y, x) <- Set.toList cells
    , y + dy > 0
    , y + dy < h
    , x + dx > 0
    , x + dx < w
    ]

-- | draw produces a string representation of the 2D Grid.
draw :: Grid -> String
draw (G h w cells) =
  unlines
    [ [d i j | j <- [0 .. w - 1]]
    | i <- [0 .. h - 1]
    ]
 where
  d i j
    | (i, j) `Set.member` cells = '#'
    | otherwise = '.'
