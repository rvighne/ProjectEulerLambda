module Euler55 where

reverseNum :: Integral i => i -> i
reverseNum num = reverseNum' num 0
  where
    reverseNum' 0 rev = rev
    reverseNum' n rev = reverseNum' q $ rev * 10 + r
      where
        (q, r) = n `quotRem` 10

palindrome :: Integral i => i -> Bool
palindrome n = n == reverseNum n

lychrel :: Integral i => i -> Bool
lychrel = all (not . palindrome) . take 50 . tail . iterate next
  where
    next n = n + reverseNum n

euler55 :: Int
euler55 = length $ filter lychrel [1..9999]
