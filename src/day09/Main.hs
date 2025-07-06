module Main where

import Data.Char (isNumber)
import Data.Foldable (find)
import Data.List (nub, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import System.Environment (getArgs)

data Block = Free | ID ID deriving (Show, Eq)

type Size = Int

type ID = Int

type File = (ID, Size)

type DiskMap = [Chunk]

data Chunk
  = -- | Free indicates a Free block
    ChunkFree Size
  | -- | Data is a contiguous Block of the given size and ID
    ChunkFile ID Size
  deriving (Show, Eq)

readMap :: String -> [Chunk]
readMap s = go s True 0
  where
    go :: String -> Bool -> Int -> [Chunk]
    go "" _ _ = []
    go (c : cs) isData currentId
      | not (isNumber c) = go cs isData currentId
      | isData = ChunkFile currentId (read [c]) : go cs (not isData) (currentId + 1)
      | otherwise = ChunkFree (read [c]) : go cs (not isData) currentId

toBlock :: Chunk -> [Block]
toBlock (ChunkFree 0) = []
toBlock (ChunkFile _ 0) = []
toBlock (ChunkFree size) = Free : toBlock (ChunkFree (size - 1))
toBlock (ChunkFile idd size) = ID idd : toBlock (ChunkFile idd (size - 1))

shiftOne :: [Block] -> [Block]
shiftOne [] = []
shiftOne bs = start ++ swapEnd middle ++ end
  where
    (start, rest) = span (/= Free) bs
    rrest = reverse rest
    (rend, rmiddle) = span (== Free) rrest
    (end, middle) = (reverse rend, reverse rmiddle)

completed :: [Block] -> Bool
completed [] = True
completed [_] = True
completed (x : y : xs) = case (x, y) of
  (Free, ID _) -> False
  (_, _) -> completed (y : xs)

swapEnd :: [a] -> [a]
swapEnd [] = []
swapEnd (x : xs) = last xs : init xs ++ [x]

showBlocks :: [Block] -> String
showBlocks = concatMap blockToStr

showMap :: [Chunk] -> String
showMap = showBlocks . concatMap toBlock

blockToStr :: Block -> String
blockToStr Free = "."
blockToStr (ID i) = show i

order :: [Block] -> [Block]
order bs = fromMaybe bs (find completed (iterate shiftOne bs))

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

-- | empty replaces the given ID with Free space
empty :: [Chunk] -> ID -> [Chunk]
empty chunks iid = go chunks
  where
    go [] = []
    go (ChunkFile fileID size : xs)
      | fileID == iid = ChunkFree size : go xs
      | otherwise = ChunkFile fileID size : go xs
    go (x : xs) = x : go xs

tryMoveChunk :: ID -> Size -> [Chunk] -> [Chunk]
tryMoveChunk _ _ [] = []
tryMoveChunk fileID size (x : xs)
  | ChunkFile fID _ <- x, fID == fileID = x : xs
  | ChunkFree emptySpace <- x, size == emptySpace = ChunkFile fileID size : emptiedTail
  | ChunkFree emptySpace <- x, size <= emptySpace = [ChunkFile fileID size, ChunkFree (emptySpace - size)] ++ emptiedTail
  | otherwise = x : tryMoveChunk fileID size xs
  where
    emptiedTail = empty xs fileID

decreasingChunks :: [Chunk] -> [(ID, Size)]
decreasingChunks cs = sortBy (comparing Data.Ord.Down) $ nub [(fileID, size) | ChunkFile fileID size <- cs]

moveAll :: [Chunk] -> [Chunk]
moveAll cs = foldl (\acc (x, y) -> tryMoveChunk x y acc) cs (decreasingChunks cs)

readBlocks :: String -> [Block]
readBlocks s = concatMap toBlock (readMap s)

partTwo :: String -> Int
partTwo s = checksum $ concatMap toBlock updatedMap
  where
    updatedMap = moveAll (readMap s)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      raw <- readFile path
      -- print $ solve raw
      print $ partTwo raw
    _ -> print "no input provided"
