import qualified Data.HashMap.Internal.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List (sort)
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

totalDist :: [(Int, Int)] -> Int
totalDist pairs = dist (sortPairs pairs)

countElems :: (Eq a, Hashable a) => [a] -> HM.HashMap a Int
countElems = foldr (\x acc -> HM.insertWith (+) x 1 acc) HM.empty

similarity :: [(Int, Int)] -> Int
similarity pairs =
  let a = map fst pairs
      bMap = countElems (map snd pairs)
      similarElements = map (\key -> key * Map.findWithDefault 0 key bMap) a
   in sum similarElements

solve :: String -> IO ()
solve path = do
  input <- readFile path
  let maybePairs = traverse parse (lines input)
  case maybePairs of
    Just pairs -> do
      print $ totalDist pairs
      print $ similarity pairs
    Nothing -> putStrLn "Invalid input file"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> solve path
    _ -> putStrLn "Please provide an input file"
