{- Project Euler
Problem 47
==========


   The first two consecutive numbers to have two distinct prime factors are:

   14 = 2 × 7
   15 = 3 × 5

   The first three consecutive numbers to have three distinct prime factors
   are:

   644 = 2² × 7 × 23
   645 = 3 × 5 × 43
   646 = 2 × 17 × 19.

   Find the first four consecutive integers to have four distinct prime
   factors. What is the first of these numbers?


   Answer: 748f517ecdc29106e2738f88aa7530f4
-}

import           Data.HashMap
import           Helpers      (primeFactorize)

enumerate :: Int -> Map Int Int
enumerate x = base $ primeFactorize x
  where
    base = foldl aggregate empty
    aggregate m el = insertWith (+) el 1 m

k = 4
check xs x n
  | n == k       = True
  | different    = check xs' x' n'
  | otherwise    = False
  where
    different  = length xs == k
    xs' = enumerate x'
    x' = x + 1
    n' = n + 1

main :: IO ()
main = print $ head [x | x <- [3..], check (enumerate x) x 0]
