{- Project Euler
Problem 10
==========


   The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

   Find the sum of all the primes below two million.


   Answer: d915b2a9ac8749a6b837404815f1ae25
-}

import           Helpers (primes)

main :: IO ()
main = print $ sum $ takeWhile (<2000000) primes
