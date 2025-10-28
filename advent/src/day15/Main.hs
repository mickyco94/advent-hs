module Main where

import MyLib
import Data.Array (Array)

type Map = Array Char

parser :: Parser Map
  

main :: IO ()
main = do
  x <- input 2024 15 parser
  print $ x
