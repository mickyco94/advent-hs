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
