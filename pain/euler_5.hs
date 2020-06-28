{- Project Euler
Problem 5
=========


   2520 is the smallest number that can be divided by each of the numbers
   from 1 to 10 without any remainder.

   What is the smallest positive number that is evenly divisible by all of
   the numbers from 1 to 20?


   Answer: bc0d0a22a7a46212135ed0ba77d22f3a
-}

import           Helpers (count, primeFactorize)

range = [1..20]

aggregate :: [Int] -> [Int] -> [Int]
aggregate lookup fs = [max (count x fs) (lookup !! (x - 1)) | x <- range]

run :: Int
run = result
  where
    result = product expanded
    expanded = [x ^ p | (p, x) <- zip primes range, p > 0]
    primes = foldl aggregate lookup (map primeFactorize range)
    lookup = [0 | _ <- range]


main :: IO ()
main = print run
