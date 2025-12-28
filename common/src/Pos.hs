module Pos where

import Data.Bifunctor (Bifunctor (first, second))

type Dir = (Int, Int)

type Position = (Int, Int)

east :: Dir
east = (0, 1)

turnLeft :: Dir -> Dir
turnLeft (dy, dx) = (-dx, dy)

turnRight :: Dir -> Dir
turnRight (dy, dx) = (dx, -dy)

step :: Position -> Dir -> Position
step (y, x) (dy, dx) = (y + dy, x + dx)

up :: Position -> Position
up = first (+ 1)

down :: Position -> Position
down = first (+ (-1))

left :: Position -> Position
left = second (+ (-1))

right :: Position -> Position
right = second (+ 1)

neighbours :: Position -> [Position]
neighbours p = map ($ p) [right, down, left, up]

--

-- | Finds the manhattan distance between two given points
mh :: Position -> Position -> Int
mh (ax, ay) (bx, by) = abs (ax - bx) + abs (by - ay)

{- | moore returns the moore neighbours of radius 1
| https://en.wikipedia.org/wiki/Moore_neighborhood
-}
moore :: Position -> [Position]
moore p =
  map
    ($ p)
    [ right
    , down
    , left
    , up
    , right . down
    , left . down
    , up . left
    , up . right
    ]
