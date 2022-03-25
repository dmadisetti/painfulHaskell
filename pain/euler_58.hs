{- Project Euler
Problem 58
==========


   Starting with 1 and spiralling anticlockwise in the following way, a
   square spiral with side length 7 is formed.

                              37 36 35 34 33 32 31
                              38 17 16 15 14 13 30
                              39 18  5  4  3 12 29
                              40 19  6  1  2 11 28
                              41 20  7  8  9 10 27
                              42 21 22 23 24 25 26
                              43 44 45 46 47 48 49

   It is interesting to note that the odd squares lie along the bottom right
   diagonal, but what is more interesting is that 8 out of the 13 numbers
   lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

   If one complete new layer is wrapped around the spiral above, a square
   spiral with side length 9 will be formed. If this process is continued,
   what is the side length of the square spiral for which the ratio of primes
   along both diagonals first falls below 10%?


   Answer: b62fc92a2561538525c89be63f36bf7b
-}

import           Helpers  (isPrime)
import Data.List

corner :: Int -> [Int]
corner start = next (start - 1) start
   where
      next delta cur = cur:next delta' (cur + delta')
         where delta' = delta + 8


scanUntil :: Int -> Int -> [(Int, Int, Int, Int)] -> Int
scanUntil p l ((a, b, c, d):xs)
   | p' <= (1 + l') `quot` 10   = (l' `quot` 2) + 1
   | otherwise                  = scanUntil p' l' xs
   where
      l' = l + 4
      p' = p + new
      new = c' a + c' b + c' c + c' d
      c' = cast . isPrime
      cast q
         | q         = 1
         | otherwise = 0

main :: IO ()
main = print $ scanUntil 0 0 $ zip4 (corner 3) (corner 5) (corner 7) (corner 9)