module Main where

import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import MyLib (Parser, input, lexeme, symbol)
import Text.Megaparsec (sepEndBy1, some)
import Text.Megaparsec.Char (letterChar, newline)

type Pattern = [Char]

type Input = ([Pattern], [Pattern])

data Trie a = Node Bool (Map.Map a (Trie a)) deriving (Show, Eq)

parser :: Parser Input
parser = do
  available <- lexeme (some letterChar) `sepEndBy1` symbol ","
  target <- some letterChar `sepEndBy1` newline
  pure (available, target)

canForm :: [Pattern] -> Pattern -> Bool
canForm _ [] = True
canForm towels target = any (canForm towels) subs
  where
    subs = [rest | t <- towels, rest <- maybeToList (stripPrefix t target)]

formable :: [Pattern] -> [Pattern] -> [Pattern]
formable towels targets = [t | t <- targets, canForm towels t]

main :: IO ()
main = do
  (patterns, target) <- input 2024 19 parser
  print (length (formable patterns target))
