module Main where

import Data.List (stripPrefix)
import System.Environment (getArgs)
import Text.Read (readMaybe)

test :: String
test = "xxmul(2,4)%"

readMuls :: String -> [(Int, Int)]
readMuls = go []
  where
    go acc [] = acc
    go acc (c : cs)
      | c == 'm' = case readMul (c : cs) of
          Just ((a, b), rest) -> go ((a, b) : acc) rest
          Nothing -> go acc cs
      | otherwise = go acc cs

-- readMul takes mul(a,b)xyz and returns ((a,b), "xyz") and Nothing otherwise
readMul :: String -> Maybe ((Int, Int), String)
readMul s = do
  afterMul <- stripPrefix "mul(" s
  let (aStr, afterA) = span (/= ',') afterMul
  afterComma <- stripPrefix "," afterA
  let (bStr, afterB) = span (/= ')') afterComma
  suffix <- stripPrefix ")" afterB
  a <- readMaybe aStr
  b <- readMaybe bStr
  return ((a, b), suffix)

solve :: String -> Int
solve = sum . map (uncurry (*)) . readMuls

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ solve contents
    _ -> print "Please provide an input file"
