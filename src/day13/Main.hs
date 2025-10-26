module Main where

import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Button = (String, Integer, Integer)

type Prize = (Integer, Integer)

type Machine = (Button, Button, Prize)

-- | parseButton produces a 'Button' object from the input string
--
-- Example:
--
-- >>> parseButton "Button A: X+94, Y+100"
-- Just ("A",94,100)
parseButton :: String -> Maybe Button
parseButton s = do
  let (fullLabel, remaining) = span (/= ':') s
  label <- stripPrefix "Button " fullLabel
  let (fullX, fullY) = span (/= ',') (drop 1 remaining)
  x <- stripPrefix " X+" fullX
  y <- stripPrefix " Y+" (drop 1 fullY)
  return (label, read x, read y)

parsePrize :: String -> Maybe Prize
parsePrize s = do
  values <- stripPrefix "Prize: " s
  let (fullX, fullY) = span (/= ',') values
  x <- stripPrefix "X=" fullX
  y <- stripPrefix ", Y=" fullY
  return (read x, read y)

parseMachine :: String -> Maybe Machine
parseMachine s = do
  let l = lines s
  buttonA <- parseButton (l !! 0)
  buttonB <- parseButton (l !! 1)
  prize <- parsePrize (l !! 2)
  return (buttonA, buttonB, prize)

parse :: String -> [Machine]
parse s = mapMaybe parseMachine (splitOn "\n\n" s)

minTokens :: Machine -> Integer
minTokens ((_, aX, aY), (_, bX, bY), (pX, pY)) = n * 3 + m
  where
    (n, m) = sim (aX, aY, bX, bY, pX, pY)

sim :: (Integer, Integer, Integer, Integer, Integer, Integer) -> (Integer, Integer)
sim (ax, ay, bx, by, x, y)
  | det == 0 = (0, 0)
  | (n, 0) <- detN `quotRem` det,
    n >= 0,
    (m, 0) <- detM `quotRem` det,
    m >= 0 =
      (n, m)
  | otherwise = (0, 0)
  where
    det = ax * by - ay * bx
    detN = x * by - bx * y
    detM = ax * y - ay * x

solve :: String -> Integer
solve s = sum (map minTokens machines)
  where
    machines = parse s

offset :: Machine -> Integer -> Machine
offset (a, b, (pX, pY)) amt = (a, b, (pX + amt, pY + amt))

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let machines = parse contents
  let offSetMachines = map (`offset` 10000000000000) machines
  print $ sum (map minTokens machines)
  print $ sum (map minTokens offSetMachines)
