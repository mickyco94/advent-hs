module Main where

import Control.Applicative (asum)
import Data.List (stripPrefix)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Token = Do | Dont | Mul Int Int deriving (Show, Eq)

readTokens :: String -> [Token]
readTokens = go []
  where
    go acc [] = reverse acc
    go acc (c : cs) = case readAction (c : cs) of
      Just (action, suffix) -> go (action : acc) suffix
      Nothing -> go acc cs

readAction :: String -> Maybe (Token, String)
readAction s = asum [readDo s, readDont s, readMul s]

readDo :: String -> Maybe (Token, String)
readDo s = do
  after <- stripPrefix "do()" s
  return (Do, after)

readDont :: String -> Maybe (Token, String)
readDont s = do
  after <- stripPrefix "don't()" s
  return (Dont, after)

readMul :: String -> Maybe (Token, String)
readMul s = do
  afterMul <- stripPrefix "mul(" s
  let (aStr, afterA) = span (/= ',') afterMul
  afterComma <- stripPrefix "," afterA
  let (bStr, afterB) = span (/= ')') afterComma
  suffix <- stripPrefix ")" afterB
  a <- readMaybe aStr
  b <- readMaybe bStr
  return (Mul a b, suffix)

parseTokens :: [Token] -> [Token]
parseTokens = go True
  where
    go _ [] = []
    go accepting (x : xs) =
      case x of
        Do -> go True xs
        Dont -> go False xs
        Mul a b
          | accepting -> Mul a b : go accepting xs
          | otherwise -> go accepting xs

mulsToTuples :: [Token] -> [(Int, Int)]
mulsToTuples xs = [(a, b) | Mul a b <- xs]

sumOfProducts :: [(Int, Int)] -> Int
sumOfProducts = sum . map (uncurry (*))

solve :: String -> Int
solve = sumOfProducts . mulsToTuples . parseTokens . readTokens

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ solve contents
    _ -> print "Please provide an input file"
