module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Position = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Position, Velocity)

type Parser = Parsec Void String

-- Define how to skip spaces and comments
spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1 -- whitespace (space, tab, newline)
    empty -- no line comments
    empty -- no block comments

-- Define lexeme & symbol helpers
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integer :: Parser Int
integer = L.signed spaceConsumer L.decimal

-- reads a,b
tuple :: Parser (Int, Int)
tuple = do
  a <- integer
  _ <- char ','
  b <- integer
  return (a, b)

-- p=0,4 v=3,-3
parseRobot :: Parser Robot
parseRobot = do
  _ <- symbol "p="
  pos <- tuple
  space
  _ <- symbol "v="
  vel <- tuple
  return (pos, vel)

parseRobots :: Parser [Robot]
parseRobots = parseRobot `sepEndBy` eol

input :: IO [Robot]
input = do
  contents <- readFile "input.txt"
  case parse parseRobots "input.txt" contents of
    Left err -> error $ errorBundlePretty err
    Right r -> return r

positionAfter :: Robot -> Int -> (Int, Int) -> Position
positionAfter ((px, py), (vx, vy)) n (boundX, boundY) =
  ( (px + n * vx) `mod` boundX,
    (py + n * vy) `mod` boundY
  )

width :: Int
width = 101

height :: Int
height = 103

-- quadrant returns
quadrant :: Position -> Int
quadrant (x, y)
  | x == middleX = -1
  | y == middleY = -1
  | x < middleX && y < middleY = 0
  | x > middleX && y < middleY = 1
  | x < middleX && y > middleY = 2
  | x > middleX && y > middleY = 3
  | otherwise = -1
  where
    middleX = (width - 1) `div` 2
    middleY = (height - 1) `div` 2

-- I want the product of the number of elements in each quadrant
safetyFactor :: [Position] -> Int
safetyFactor positions = product (Map.elems excludingMiddle) -- Need to exclude middle...
  where
    counter = countBy quadrant positions
    excludingMiddle = Map.filterWithKey (\k _ -> k /= -1) counter

-- countBy accumulates objects by a predicate, returning a map of predicate result -> count
countBy :: (Ord k) => (a -> k) -> [a] -> Map.Map k Int
countBy f = foldl (\m x -> Map.insertWith (+) (f x) 1 m) Map.empty

formsLine :: [Position] -> Int
formsLine positions = maximum lineLengths
  where
    lineLengths = map (`line` positions) positions

line :: Position -> [Position] -> Int
line start positions = go start 0
  where
    pSet = Set.fromList positions
    go (x, y) curr
      | (x, y) `Set.member` pSet = go (x + 1, y) curr + 1
      | otherwise = curr

display :: [Position] -> String
display positions = unlines [[c |x <- [0 .. width], let c = if (x,y) `elem` positions then '*' else '.']  | y <- [0 .. height]]

main :: IO ()
main = do
  robots <- input
  let positions n = [positionAfter robot n (width, height) | robot <- robots]
  print $ safetyFactor (positions 100)
  -- Takes a long time...
  print $ head [n | n <- [100 .. 10000], let nThSet = positions n, formsLine nThSet > 5]
