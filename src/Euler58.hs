module Euler58 where

import Data.List (elemIndex)

-- | Get the numbers on the diagonal corners of a n-sided spiral
-- | (except for the bottom-right, since it's always composite)
-- | n must be odd
diagonals :: Int -> [Int]
diagonals n = [start - i * diff | i <- [1..3]]
  where
    start = n * n
    diff = n - 1

-- | Infinite list of the odd prime numbers
oddprimes :: [Int]
oddprimes = 3 : filter prime [5,7..]

-- | Test for primality by trial division with primes
-- | n mustn't be even (which is true for the diagonals)
prime :: Int -> Bool
prime n = foldr notFactors True oddprimes
  where
    notFactors p rest = p * p > n || n `rem` p /= 0 && rest

-- | Solve Project Euler problem #58
euler58 :: Int
euler58 = 1 + 2 * (idx + 1)
  where
    Just idx = elemIndex True $ zipWith underRatio counts [5,9..]
    count `underRatio` total = count * 10 < total
    counts = scanl1 (+) $ length . filter prime . diagonals <$> [3,5..]
