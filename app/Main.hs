module Main where

import Euler81

main :: IO ()
main = getMatrix >>= print . euler81
