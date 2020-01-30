module Euler50 where

import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad

sieve :: Int -> UArray Int Bool
sieve max = runSTUArray $ do
  sieve <- newArray (2, max) True
  forM_ (2:[3,5..max]) $ \p -> do
    isPrime <- readArray sieve p
    when isPrime $ forM_ [p*2,p*3..max] $ \f -> writeArray sieve f False
  return sieve

psumsp :: Int -> [Int]
psumsp max = filter (sv!) $ takeWhile (<max) $ scanl1 (+) ps
  where
    sv = sieve max
    ps = [n | (n, p) <- assocs sv, p]

euler50 :: Int
euler50 = undefined--scanr1 (+) $ primess 1000
