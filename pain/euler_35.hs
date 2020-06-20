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
import Data.Char
import GHC.Float
import Helpers (primes, is_prime)

log10 :: Int -> Int
log10 = float2Int . logBase 10 . int2Float

is_circular_prime :: Int -> Bool
is_circular_prime n = length (take (l + 1) (n:circ n)) == (l + 1)
  where
    l = log10 n
    circ x
      | x < 10      = x:circ x
      | is_prime x' = x':circ x'
      | otherwise   = []
      where
        x' = quot x 10 + (10 ^ l) * (mod x 10)

evenContaining :: Int -> Bool
evenContaining = getAny .
                    mconcat .
                        map (Any . not . easily_divisible) .
                            (map digitToInt) . show
                  where
                    easily_divisible n = even n || n == 5

main :: IO ()
main = print $ (+) 2 $ length $ filter is_circular_prime $ filter evenContaining (takeWhile (<1000000) primes)
