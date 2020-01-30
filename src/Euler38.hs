module Euler38 where

import Data.List (find, foldl1', isPrefixOf)

-- | List representation of the digits of an integer
type Digits = [Int]

-- | Number representation of a list of digits.
fromDigits :: Digits -> Int
fromDigits = foldl1' appendDigit
  where
    appendDigit num digit = num * 10 + digit;

-- | Lexicographic permutations.
permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = [ y:zs | (y,ys) <- select xs, zs <- permutations ys]
  where select []     = []
        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

-- | List of n-digit pandigital numbers in descending order.
pandigitals :: Int -> [Digits]
pandigitals n = permutations [n,n-1..1]

-- | All possible ways to slice a n-digit number in descending order.
getSlices :: Int -> [[Int]]
getSlices 0 = []
getSlices n = [n] : concatMap f [1..n]
  where
    f x = [x:xs | xs <- getSlices (n - x), head xs >= x]

-- | Split a list into groups according to a "template" of run-lengths.
splitPlaces :: [Int] -> [Int] -> [[Int]]
splitPlaces [] _ = []
splitPlaces xs (ln:lns) = part : splitPlaces rest lns
  where
    part = take ln xs
    rest = drop ln xs

{-
splitPlaces :: Int -> [Int] -> [Int]
splitPlaces num lns = reverse $ splitPlaces' lns num

splitPlaces' :: [Int] -> Int -> [Int]
splitPlaces' [] _ = []
splitPlaces' (ln:lns) num = part : splitPlaces' lns rest
  where
    idx = 10 ^ ln
    part = num `rem` idx
    rest = num `quot` idx
    -- (rest, part) = num `quotRem` (10 ^ ln)
-}

-- | Multiply a number by these to get a concatenated product.
factors :: [Int]
factors = [2..]

-- | Whether the numbers are products of the list (1, 2, ...).
isProdList :: [Int] -> Bool
isProdList (n:prods) = prods `isPrefixOf` ((n*) <$> factors)

-- | Number of digits in the pandigital number for Project Euler problem #38.
kDigits :: Int
kDigits = 9

-- | Whether a k-digit number is a concatenated product.
isConcatProd :: [Int] -> Bool
isConcatProd digits = any isProdList [fromDigits <$> split | split <- splitPlaces digits <$> tail (getSlices kDigits)]

-- | Solve Project Euler problem #38.
euler38 :: Int
euler38
  | Just n <- find isConcatProd (pandigitals kDigits) = fromDigits n
  | otherwise = error "No solution" -- Should never occur
