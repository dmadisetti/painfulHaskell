{- Project Euler
Problem 6
=========


   The sum of the squares of the first ten natural numbers is,

                          1^2 + 2^2 + ... + 10^2 = 385

   The square of the sum of the first ten natural numbers is,

                       (1 + 2 + ... + 10)^2 = 55^2 = 3025

   Hence the difference between the sum of the squares of the first ten
   natural numbers and the square of the sum is 3025 − 385 = 2640.

   Find the difference between the sum of the squares of the first one
   hundred natural numbers and the square of the sum.


   Answer: 867380888952c39a131fe1d832246ecc
-}

import Helpers (triangle)

square_triangles a = sum [x ^ 2 | x <- [1..a]]


main :: IO ()
main = print $ (triangle 100) ^ 2 - (square_triangles 100)
