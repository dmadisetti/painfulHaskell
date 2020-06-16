{- Project Euler
Problem 28
==========


   Starting with the number 1 and moving to the right in a clockwise
   direction a 5 by 5 spiral is formed as follows:

                                 21 22 23 24 25
                                 20  7  8  9 10
                                 19  6  1  2 11
                                 18  5  4  3 12
                                 17 16 15 14 13

   It can be verified that the sum of the numbers on the diagonals is 101.

   What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
   formed in the same way?


   Answer: 0d53425bd7c5bf9919df3718c8e49fa6
-}

x = 500

-- Crunched and then reduced in Wolfram
main :: IO ()
main = print $ (16 * x ^ 3 + 30 * x ^ 2 + 26 * x + 3) `div` 3
