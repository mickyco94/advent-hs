module Main where

import Data.Char (isNumber)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

data Block = Free | ID ID deriving (Show, Eq)

type Size = Int

type ID = Int

data DiskMap
  = -- | Free indicates a Free block
    FreeMap Size
  | -- | Data is a contiguous Block of the given size and ID
    DataMap ID Size
  deriving (Show, Eq)

readMap :: String -> [DiskMap]
readMap s = go s True 0
  where
    go :: String -> Bool -> Int -> [DiskMap]
    go "" _ _ = []
    go (c : cs) isData currentId
      | not (isNumber c) = go cs isData currentId
      | isData = DataMap currentId (read [c]) : go cs (not isData) (currentId + 1)
      | otherwise = FreeMap (read [c]) : go cs (not isData) currentId

toBlock :: DiskMap -> [Block]
toBlock (FreeMap 0) = []
toBlock (DataMap _ 0) = []
toBlock (FreeMap size) = Free : toBlock (FreeMap (size - 1))
toBlock (DataMap idd size) = ID idd : toBlock (DataMap idd (size - 1))

shiftOne :: [Block] -> [Block]
shiftOne [] = []
shiftOne (b : bs) = case b of
  Free -> last bs : init bs ++ [Free]
  ID _ -> b : shiftOne bs

shiftOneSpan :: [Block] -> [Block]
shiftOneSpan [] = []
shiftOneSpan bs = start ++ swapEnd middle ++ end
  where
    (start, rest) = span (/= Free) bs
    rrest = reverse rest
    (rend, rmiddle) = span (== Free) rrest
    (end, middle) = (reverse rend, reverse rmiddle)

completed :: [Block] -> Bool
completed [] = True
completed (x : xs) = case x of
  Free -> not $ isId `any` xs
  _ -> completed xs

isId :: Block -> Bool
isId (ID _) = True
isId _ = False

swapEnd :: [a] -> [a]
swapEnd [] = []
swapEnd (x : xs) = last xs : init xs ++ [x]

showBlocks :: [Block] -> String
showBlocks = concatMap blockToStr

blockToStr :: Block -> String
blockToStr Free = "."
blockToStr (ID i) = show i

order :: [Block] -> [Block]
order bs = fromMaybe bs (find completed (iterate shiftOneSpan bs))

checksum :: [Block] -> Int
checksum blocks = go (zip [0 ..] blocks) 0
  where
    go :: [(Int, Block)] -> Int -> Int
    go [] c = c
    go ((i, b) : bs) c = case b of
      Free -> go bs c
      ID iid -> go bs (c + (i * iid))

solve :: String -> Int
solve s = checksum (order blocks)
  where
    diskMap = readMap s
    blocks = concatMap toBlock diskMap

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      print $ solve raw
    _ -> print "no input provided"
