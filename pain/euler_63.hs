{- Project Euler
Problem 63
==========


   The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the
   9-digit number, 134217728=8^9, is a ninth power.

   How many n-digit positive integers exist which are also an nth power?


   Answer: f457c545a9ded88f18ecee47145a72c0
-}


main :: IO ()
main = print $ sum [floor $ logBase (10 / x) 10 | x <- [1..9] :: [Double]]