module Lib
    (
    closest
    ) where

import Text.EditDistance
import Data.List

dist :: [Char] -> [Char] -> Int
-- Uses levenshtein distance to calulate the edit distance between 2 strings.
-- O(nm) where n and m are the length of the 2 strings
dist x y = levenshteinDistance defaultEditCosts x y

distanceCombos :: [[Char]] -> [(Int, [Char], [Char])]
-- creates a list of triples where first element is the edit distance and the next 2 are the compared strings
distanceCombos l = [(dist x y, x, y) | (x:ys) <- tails l, y <- ys] -- not sure how efficient "tails" is on large lists

bestFrom (distAB, a, b) (distXY, x, y) =
  if distAB < distXY
    then (distAB, a, b)
    else (distXY, x, y)

closest :: [[Char]] -> ([Char], [Char])
closest [] = error "Nothing to compare"
closest [x] = error ("Nothing to compare to " ++ x)
closest [x,y] = (x, y)
closest all@(x:y:xs) =
  let startingDist = maxBound :: Int
      (_,a,b) = foldl (\bestCombo nextCombo -> bestFrom bestCombo nextCombo) -- folding over them each pair is O((n^2 + n) / 2) (i.e. nth triangular number)
                      (startingDist, "", "")
                      (distanceCombos all) -- creating the pairs is O((n^2 + n) / 2) (i.e. nth triangular number)
  in (a,b)
