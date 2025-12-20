{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import MyLib (Parser, input, integer, lexeme, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline)
import Prelude

data Light = On | Off deriving (Show, Eq, Ord)

data Machine
  = Machine
      [Light] -- Indicator lights
      [[Int]] -- Buttons
      [Int] -- Joltage
  deriving (Show)

-- e.g. [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
parser :: Parser [Machine]
parser = parseMachine `sepEndBy1` newline

toggle :: Light -> Light
toggle Off = On
toggle On = Off

press :: [Int] -> [Light] -> [Light]
press buttons lights = zipWith (curry t) lights [0 ..]
  where
    t (l, idx)
      | idx `elem` buttons = toggle l
      | otherwise = l

initialLights :: [Light] -> [Light]
initialLights = map (const Off)

initialJoltage :: [Int] -> [Int]
initialJoltage = map (const 0)

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc t -> acc * 10 + t) 0

-- buttonsToInt 2 [0] = 10
buttonsToInt :: Int -> [Int] -> Int
buttonsToInt n xs = digitsToInt [fromEnum (i `elem` xs) | i <- [0 .. n - 1]]

parseMachine :: Parser Machine
parseMachine =
  Machine
    <$> lights
    <*> buttons
    <*> joltage
  where
    commaSeparatedInts = integer `sepBy1` char ','
    buttons = some (between (symbol "(") (symbol ")") commaSeparatedInts)
    joltage = between (char '{') (char '}') commaSeparatedInts
    lights = map toLight <$> lexeme (between (symbol "[") (symbol "]") (many (oneOf ".#")))
    toLight '#' = On
    toLight _ = Off

minPresses :: Machine -> Maybe Int
minPresses (Machine target buttons _) = go [(0, initialLights target)] Map.empty
  where
    go :: [(Int, [Light])] -> Map.Map [Light] Int -> Maybe Int
    go [] counter = Map.lookup target counter
    go ((depth, x) : xs) counter
      | Just existing <- Map.lookup x counter, existing <= depth = go xs counter
      | otherwise = go (xs ++ next') counter'
      where
        next = map (`press` x) buttons
        next' = map (depth + 1,) next
        counter' = Map.insert x depth counter

minPressesJoltage :: Machine -> Maybe Int
minPressesJoltage (Machine _ buttons target) = go [(0, targetInt)] IntMap.empty
  where
    targetInt = digitsToInt target
    n = length target
    buttonsAsInts = map (buttonsToInt n) buttons

    go [] counter = IntMap.lookup 0 counter
    go ((depth, x) : xs) counter
      | Just best <- IntMap.lookup 0 counter, depth >= best = go xs counter
      | x < 0 = go xs counter
      | Just existing <- IntMap.lookup x counter, existing <= depth = go xs counter
      | otherwise = go (next ++ xs) counter'
      where
        next = map (\t -> (depth + 1, x - t)) buttonsAsInts
        counter' = IntMap.insert x depth counter

main :: IO ()
main = do
  machines <- input 2025 10 parser

  print (sum (mapMaybe minPresses machines))
  print (sum (mapMaybe minPressesJoltage machines))
