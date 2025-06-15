module Main where

import GHC.Event (updateTimeout)
import System.Environment (getArgs)

type Page = Int

-- A|B
type Rule = (Page, Page)

-- A,B,C
type Update = [Page]

data Token = Rule | Update

readRule :: String -> Rule
readRule s = (read a, read (drop 1 b))
  where
    (a, b) = break (== '|') s

readUpdate :: String -> Update
readUpdate = map read . splitAll ','

splitAll :: Char -> String -> [String]
splitAll _ "" = []
splitAll delim s =
  let (a, b) = break (== delim) s
   in a : case b of
        [] -> []
        (_ : rest) -> splitAll delim rest

tokenise :: String -> ([Rule], [Update])
tokenise s =
  let lineSet = lines s
      (before, rest) = break (== "") lineSet
      after = drop 1 rest
      rules = map readRule before
      updates = map readUpdate after
   in (rules, updates)

solve :: String -> Int
solve s = sum (map middle (validUpdates updates rules))
  where
    (rules, updates) = tokenise s

valid :: Update -> [Rule] -> Bool
valid update rules = go update []
  where
    go [] _ = True
    go (u : us) prev
      | anyCommon (map (\x -> (u, x)) prev) rules = False
      | otherwise = go us (u : prev)

validUpdates :: [Update] -> [Rule] -> [Update]
validUpdates updates rules = [u | u <- updates, valid u rules]

middle :: [a] -> a
middle xs = xs !! mid
  where
    len = length xs
    mid = div len 2

-- Use a Set
anyCommon :: (Eq a) => [a] -> [a] -> Bool
anyCommon xs ys = any (`elem` ys) xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ solve contents
    _ -> print "Please provide an input file"
