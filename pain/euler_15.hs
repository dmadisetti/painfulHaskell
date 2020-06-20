{- Project Euler
Problem 15
==========


   Starting in the top left corner of a 2×2 grid, and only being able to move
   to the right and down, there are exactly 6 routes to the bottom right
   corner.

   How many such routes are there through a 20×20 grid?


   p_015.gif
   Answer: 928f3957168ac592c4215dcd04e0b678
-}

import           Helpers (factorial)

permute :: Integer -> Integer -> Integer
permute n 0 = 1
permute n k = n * permute (n-1) (k-1)

choose :: Integer -> Integer -> Integer
choose n k = quot (permute n k) (factorial k)

main :: IO ()
main = print (choose 40 20)
