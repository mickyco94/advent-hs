module Pos where

type Dir = (Int,Int)
type Position = (Int,Int)

east :: Dir
east = (0, 1)

turnLeft :: Dir -> Dir
turnLeft (dy,dx) = (-dx ,dy)

turnRight :: Dir -> Dir
turnRight (dy,dx) = (dx,-dy)

step :: Position -> Dir -> Position
step (y, x) (dy, dx) = (y + dy, x + dx)
