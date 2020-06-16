{- Project Euler
Problem 14
==========


   The following iterative sequence is defined for the set of positive
   integers:

   n → n/2 (n is even)
   n → 3n + 1 (n is odd)

   Using the rule above and starting with 13, we generate the following
   sequence:

                   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

   It can be seen that this sequence (starting at 13 and finishing at 1)
   contains 10 terms. Although it has not been proved yet (Collatz Problem),
   it is thought that all starting numbers finish at 1.

   Which starting number, under one million, produces the longest chain?

   NOTE: Once the chain starts the terms are allowed to go above one million.


   Answer: 5052c3765262bb2c6be537abd60b305e
-}

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n      = n:collatz (quot n 2)
  | otherwise   = n:collatz (3*n + 1)


main :: IO ()
main = print $ snd $ maximum [(length $ collatz n, n) | n <- [1..1000000]]
