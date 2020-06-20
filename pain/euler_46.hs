{- Project Euler
Problem 46
==========


   It was proposed by Christian Goldbach that every odd composite number can
   be written as the sum of a prime and twice a square.

   9 = 7 + 2×1^2
   15 = 7 + 2×2^2
   21 = 3 + 2×3^2
   25 = 7 + 2×3^2
   27 = 19 + 2×2^2
   33 = 31 + 2×1^2

   It turns out that the conjecture was false.

   What is the smallest odd composite that cannot be written as the sum of a
   prime and twice a square?


   Answer: 89abe98de6071178edb1b28901a8f459
-}

import Helpers (primes, is_divisible)

merge :: [Int] -> [Int] -> [Int] -> Int
merge sq@(s:ss) pm@(p:ps) od@(o:os)
    | s * 2 > o    = o
    | attempt < o  = merge sq ps od
    | attempt > o  = merge ss primes od
    | otherwise    = merge squares primes os
  where
    attempt = p + 2 * s

squares :: [Int]
squares = 1:sq 2
  where
    sq n = n*n:sq (n + 1)

odds :: [Int]
odds = filter is_divisible [3,5..]

main :: IO ()
main = print $ merge squares primes odds
