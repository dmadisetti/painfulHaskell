{- Project Euler
Problem 4
=========


   A palindromic number reads the same both ways. The largest palindrome made
   from the product of two 2-digit numbers is 9009 = 91 × 99.

   Find the largest palindrome made from the product of two 3-digit numbers.


   Answer: d4cfc27d16ea72a96b83d9bdef6ce2ec
-}

import           Helpers (is_int_palindrome)

main :: IO ()
main = print $ maximum [ans |
    x <- [100..999],
    y <- [100..999],
    let ans = x * y,
    is_int_palindrome ans]

