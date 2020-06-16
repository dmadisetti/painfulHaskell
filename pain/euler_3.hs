{- Project Euler
Problem 3
=========


   The prime factors of 13195 are 5, 7, 13 and 29.

   What is the largest prime factor of the number 600851475143 ?


   Answer: 94c4dd41f9dddce696557d3717d98d82
-}

import Helpers (prime_factorize)

main :: IO ()
main = print $ maximum (prime_factorize 600851475143)
