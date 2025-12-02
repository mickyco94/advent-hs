module Main where

import MyLib (Parser, input, integer)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char)
import Prelude

-- Range is upper and lower inclusive
type Range = (Int, Int)

parser :: Parser [Range]
parser = line `sepEndBy` comma
  where
    comma = char ','
    line = do
      start <- integer
      _ <- char '-'
      end <- integer
      pure (start, end)

invalid :: Int -> Bool
invalid x = even l && pre == end
  where
    s = show x
    l = length s
    (pre, end) = splitAt (l `div` 2) s

-- | allEqual determines if all elements in a List are equal
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

repeating :: (Show a) => a -> Bool
repeating a = any allEqual (partitions (show a))

-- | partitions produces all partitions of the list
partitions :: [a] -> [[[a]]]
partitions xs = [partition xs n | n <- [1 .. (length xs `div` 2)]]

-- | partition breaks the list into partitions of size n
partition :: [a] -> Int -> [[a]]
partition xs n = [take n (drop d xs) | d <- [0, n .. (length xs - 1)]]

asRange :: Range -> [Int]
asRange (a, b) = [a .. b]

main :: IO ()
main = do
  ids <- input 2025 2 parser
  let values = concatMap asRange ids
  print (sum (filter invalid values))
  print (sum (filter repeating values))
