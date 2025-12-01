module Main where

import MyLib (Parser, input, integer)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (char, eol)
import Prelude

type Instruction = Int

parser :: Parser [Instruction]
parser = line `sepEndBy` eol
  where
    line =
      char 'L'
        *> (negate <$> integer)
          <|> (char 'R' *> integer)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

passZero :: Int -> Int -> Int
passZero x amt = abs ((x + amt) `div` 100)

main :: IO ()
main = do
  arr <- input 2025 1 parser
  let turns = map (`mod` 100) (scanl (+) 50 arr)
      zeroes = zipWith passZero turns arr
  print (count (== 0) turns)
  print (sum zeroes)
