module Main where

import Data.List (intersect)
import System.Environment (getArgs)

type Page = Int

type Rule = (Page, Page)

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

-- Update this to return the Rule it breaks
valid :: Update -> [Rule] -> Bool
valid update rules = null (rulesBroken update rules)

rulesBroken :: Update -> [Rule] -> [Rule]
rulesBroken update rules = go update [] []
  where
    go [] _ brokenRules = brokenRules
    go (u : us) prev brokenRules = go us (u : prev) brokenRules ++ newBrokenRules
      where
        prevRuleSet = map (\x -> (u, x)) prev
        newBrokenRules = prevRuleSet `intersect` rules

validUpdates :: [Update] -> [Rule] -> [Update]
validUpdates updates rules = [u | u <- updates, valid u rules]

invalidUpdates :: [Update] -> [Rule] -> [Update]
invalidUpdates updates rules = [u | u <- updates, not (valid u rules)]

middle :: [a] -> a
middle xs = xs !! mid
  where
    len = length xs
    mid = div len 2

anyCommon :: (Eq a) => [a] -> [a] -> Bool
anyCommon xs ys = any (`elem` ys) xs

-- | 'insertUpdate' inserts the page into the update while respecting the rules
--
-- Example:
-- >>> insertUpdate 
insertUpdate :: [Rule] -> Update -> Page -> Update
insertUpdate rules xs x =
  let idx = length $ takeWhile (\v -> (x, v) `notElem` rules) xs
   in take idx xs ++ [x] ++ drop idx xs

fixUpdate :: Update -> [Rule] -> Update
fixUpdate update rules = foldl (insertUpdate rules) [] update

solvePartTwo :: [Update] -> [Rule] -> Int
solvePartTwo updates rules =
  let fixedRules = map (`fixUpdate` rules) (invalidUpdates updates rules)
   in sum (map middle fixedRules)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      let (rules, updates) = tokenise contents
      print $ solvePartTwo updates rules
    _ -> print "Please provide an input file"
