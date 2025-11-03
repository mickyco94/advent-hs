{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bits (Bits (shift, xor))
import MyLib (Parser, input, integer)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, newline, string)

data Program = Program
  { regA :: Int,
    regB :: Int,
    regC :: Int,
    instructions :: [Int],
    pointer :: Int,
    output :: [Int]
  }
  deriving (Show)

parser :: Parser Program
parser = do
  _ <- string "Register A: "
  a <- integer
  _ <- newline
  _ <- string "Register B: "
  b <- integer
  _ <- newline
  _ <- string "Register C: "
  c <- integer
  _ <- newline >> newline
  _ <- string "Program: "
  ins <- integer `sepBy` char ','
  pure $ Program {regA = a, regB = b, regC = c, instructions = ins, pointer = 0, output = []}

combo :: Program -> Int -> Int
combo _ x | 0 <= x, x <= 3 = x
combo Program {regA = a} 4 = a
combo Program {regB = b} 5 = b
combo Program {regC = c} 6 = c
combo _ _ = error "Unsupported usage!"

run :: Program -> Program
run p@Program {..} = case rest of
  [] -> p
  0 : op : _ -> run . advance $ p {regA = regA `quot` 2 ^ combo p op}
  1 : op : _ -> run . advance $ p {regB = regB `xor` op}
  2 : op : _ -> run . advance $ p {regB = combo p op `mod` 8}
  3 : _ : _ | regA == 0 -> run . advance $ p
  3 : op : _ -> run p {pointer = op}
  4 : _ : _ -> run . advance $ p {regB = regB `xor` regC}
  5 : op : _ -> run . advance $ p {output = output ++ [combo p op `mod` 8]}
  6 : op : _ -> run . advance $ p {regB = regA `quot` 2 ^ combo p op}
  7 : op : _ -> run . advance $ p {regC = regA `quot` 2 ^ combo p op}
  _ -> error "unsupported instructions"
  where
    advance p' = p' {pointer = pointer + 2}
    rest = drop pointer instructions

lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

findMin :: Program -> Maybe Int
findMin p = case go 1 [0] of
  [] -> Nothing
  candidates -> Just $ minimum candidates
  where
    go :: Int -> [Int] -> [Int]
    go n candidates
      | n > length (instructions p) = candidates
      | otherwise = go (n + 1) candidates'
      where
        runout a = output (run p {regA = a})
        instructionTail = takeLast n (instructions p) 
        candidates' = [a' | a <- candidates, i <- [0 .. 8], let a' = shiftOctal a + i, instructionTail == runout a']

shiftOctal :: Int -> Int
shiftOctal a = a `shift` 3

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

main :: IO ()
main = do
  program <- input 2024 17 parser
  print (output (run program))
  print (findMin program)
