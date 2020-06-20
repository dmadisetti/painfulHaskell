{- Project Euler
Problem 7
=========


   By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
   that the 6th prime is 13.

   What is the 10 001st prime number?


   Answer: 8c32ab09ec0210af60d392e9b2009560
-}

import           Helpers (primes)

main :: IO ()
main = print $ primes !! 10000
