module Euler97 where

euler97 :: Int
euler97 = 1 + iterate double 28433 !! 7830457
  where
    double = (`rem` 10000000000) . (2*)
