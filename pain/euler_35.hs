{- Project Euler
Problem 35
==========


   The number, 197, is called a circular prime because all rotations of the
   digits: 197, 971, and 719, are themselves prime.

   There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
   71, 73, 79, and 97.

   How many circular primes are there below one million?


   Answer: b53b3a3d6ab90ce0268229151c9bde11
-}

import Data.Monoid
import GHC.Float
import Helpers (primes, is_prime)

log10 :: Int -> Int
log10 = float2Int . logBase 10 . int2Float

is_circular :: Int -> Bool
is_circular n = length (take (l + 1) (n:circ n)) == (l + 1)
  where
    l = log10 n
    circ x
      | x < 10      = [x]
      | is_prime x' = x':circ x'
      | otherwise   = []
      where
        x' = quot x 10 + (10 ^ l) * (mod x 10)

evenContaining :: Int -> Bool
evenContaining n = getAny $ mconcat [Any $ not $ even (digit) || (mod digit 5 == 0) | i <- [0..l], let digit = n `div` (10^i)]
  where
    l = log10 n

main :: IO ()
main = print $ 2 + sum [1 | x <- filter evenContaining (takeWhile (<1000000) primes), is_circular x]
