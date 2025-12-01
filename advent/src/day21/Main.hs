module Main where

import qualified Data.Map as Map
import Pos (Position)

-- Numeric keypad
--
-- +---+---+---+

-- | 7 | 8 | 9 |
-- +---+---+---+
-- | 4 | 5 | 6 |
-- +---+---+---+
-- | 1 | 2 | 3 |
-- +---+---+---+
--     | 0 | A |
--     +---+---+
-- Directional pad
--     +---+---+
--     | ^ | A |
-- +---+---+---+
-- | < | v | > |
-- +---+---+---+
--
-- The order of control is:
-- Me (Directional)
-- -> Robot (Directional)
-- -> Robot (Directional)
-- -> Robot (NumPad)
--
-- Starting position in all cases is A
--
-- e.g. to press 029A:
-- Me:    <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
-- Robot: v<<A>>^A<A>AvA<^AA>A<vAAA>^A
-- Robot: <A^A>^^AvvvA
-- Robot: 029A
--
-- We need to find the shortest sequence to get the final robot to press a given sequence.
shortest :: String -> Int
shortest "029A" = 15
shortest _ = -1

numpad :: Map.Map Char Position
numpad =
  Map.fromList
    [ ('_', (0, 0)),
      ('0', (0, 1)),
      ('A', (0, 2)),
      ('7', (1, 0)),
      ('8', (1, 1)),
      ('9', (1, 2)),
      ('4', (2, 0)),
      ('5', (2, 1)),
      ('6', (2, 2)),
      ('7', (3, 0)),
      ('8', (3, 1)),
      ('9', (3, 2))
    ]

-- Let's start with going up one level. I guess I could represent a position in 2D?
-- And then the shortest path

-- | move moves from a to b on a numpad
move :: Char -> Char -> [String]
move '0' '9' = ["^^^>A", ">^^^A", "^>^^A"] -- Or more generally, I need one right and 3 ups. In no particular order
-- So given a distance horizontally and vertically. The move is just a set of ^><v counted various times

-- With a:b:rest I could find a way to go from a -> b
-- I need a way to represent the distances between the positions

main :: IO ()
main = putStrLn "Day 21 not implemented yet"
