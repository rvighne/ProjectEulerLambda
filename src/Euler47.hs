module Euler47 where

import Data.List (find)

-- | List of prime numbers
primes :: Integral i => [i]
primes = sieve $ 2 : [3,5..]
  where
    sieve (p:xs) = p : sieve (filter notComposite xs)
      where
        notComposite x = x `rem` p /= 0

{-
-- | Prime factors
pfactors :: Integral i => i -> [i]
pfactors n = pfactors' n primes
  where
    pfactors' n pps@(p:ps)
      | p == n = [n]
      | r == 0 = p : pfactors' q pps
      | otherwise = pfactors' n ps
      where
        (q, r) = n `quotRem` p
-}

dfactors :: Integral i => i -> i
dfactors n = dfactors' n primes
  where
    dfactors' n (p:ps)
      | n == 1 = 0
      | p == n = 1
      | d /= 1 = 1 + dfactors' (n `quot` d) ps
      | otherwise = dfactors' n ps
      where
        d = until ((/=0) . (n `rem`)) (*p) p `quot` p

-- | Number of consecutive numbers having this number of distinct prime factors required by Project Euler problem #47
nFactors :: Integral i => i
nFactors = 4

-- | Solve Project Euler problem #47 within the given range of numbers
euler47' :: Integral i => [i] -> i -> i
euler47' (x:xs) n
  | n == nFactors = x - n
  | dfactors x == nFactors = euler47' xs (n + 1)
  | otherwise = euler47' xs 0

-- | Solve Project Euler problem #47
euler47 :: Int
euler47 = euler47' [product (take nFactors primes) ..] 0
