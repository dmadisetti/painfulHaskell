{- Project Euler
Problem 53
==========


   There are exactly ten ways of selecting three from five, 12345:

              123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

   In combinatorics, we use the notation, ^5C[3] = 10.

   In general,

      ^nC[r] =    n!    ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
               r!(n−r)!

   It is not until n = 23, that a value exceeds one-million: ^23C[10] =
   1144066.

   How many, not necessarily distinct, values of  ^nC[r], for 1 ≤ n ≤ 100,
   are greater than one-million?


   Answer: e3b21256183cf7c2c7a66be163579d37
-}
import           Helpers (factorial)

choose :: Integer -> Integer -> Integer
choose n = quot (factorial n) . denom
   where denom r = (*) (factorial r) (factorial $ (-) n r)

calc limit needle = calc' 0
   where
      calc' result n r
         | n <= limit  = calc' result' n' r'
         | otherwise = result
         where
            result' = (result + (1 + n - 2 * r'))
            n' = n + 1
            r' = takeUntil $ r - 1
            takeUntil x
               | choosen x < needle = x + 1
               | otherwise          = takeUntil $ x - 1
            choosen = choose n

search limit needle = search'
   where
      search' n
         | choose n r < needle   = search' (n + 1)
         | otherwise             = calc' n r
         where r = (quot n 2)
      calc' = calc limit needle

main :: IO ()
main = print $ search 100 1000000 1