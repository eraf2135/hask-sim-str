module Main where

import Lib

main :: IO ()
main = do
  contents <- getContents
  let (x,y) = closest (lines contents)
  putStrLn (x)
  putStrLn (y)