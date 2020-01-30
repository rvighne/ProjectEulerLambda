module Euler81 where

import Data.Array.Unboxed
import Data.List.Split (splitOneOf)
import Data.Sequence

type Idx = Int
type Cell = Int
type Matrix = UArray (Idx, Idx) Cell

size :: Idx
size = 80

getMatrix :: IO Matrix
getMatrix = parse <$> readFile "data/p081_matrix.txt"
  where
    parse = listArray ((1,1),(size,size)) . map read . splitOneOf ",\n"

calc :: Idx -> Idx -> Matrix -> Seq Cell
calc i j _
  | i > size || j > size = empty
calc i j m
  | i == size && j == size = singleton $ m ! (i,j)
calc i j m = (cur+) <$> (down >< right)
  where
    cur = m ! (i,j)
    down = calc (i + 1) j m
    right = calc i (j + 1) m

euler81 :: Matrix -> Cell
euler81 = minimum . calc 1 1
