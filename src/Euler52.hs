module Euler52 where

default (Int)

-- | Are all of the given same-sized sets the same?
-- | Must not be called with empty list
sameSets :: Eq e => [[e]] -> Bool
sameSets [_] = True
sameSets (a:b:rest)
  | all (`elem` a) b = sameSets (b:rest)
  | otherwise = False

-- | Solve Project Euler problem #52
euler52 :: Int
euler52 = head [x | d <- [1..], x <- [1..10^d `quot` 6], hasPerMults x]
  where
    hasPerMults x = sameSets $ (show . (x*)) <$> [2..6]
