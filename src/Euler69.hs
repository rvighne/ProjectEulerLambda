module Euler69 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Function (on)

ratio :: Int -> Double
ratio n = n `fdiv` totient n
  where
    fdiv = (/) `on` fromIntegral

totient :: Int -> Int
totient n = length $ filter (==1) $ gcd n <$> [1..n-1]

euler69 :: Int
euler69 = maximumBy (comparing ratio) [2..100000]
