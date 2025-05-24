import Data.List (sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

parse :: String -> Maybe (Int, Int)
parse line =
  case words line of
    [l, r] -> do
      lInt <- readMaybe l
      rInt <- readMaybe r
      return (lInt, rInt)
    _ -> Nothing

sortPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
sortPairs pairs =
  let a = (sort . map fst) pairs
      b = (sort . map snd) pairs
   in zip a b

dist :: [(Int, Int)] -> Int
dist = sum . map diff

diff :: (Int, Int) -> Int
diff (x, y) = abs (x - y)

solve :: String -> Maybe Int
solve contents = do
  pairs <- traverse parse (lines contents)
  let sortedPairs = sortPairs pairs
  return (dist sortedPairs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      print $ fromMaybe 0 (solve contents)
    _ -> putStrLn "Please provide an input file"
