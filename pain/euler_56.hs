{- Project Euler
Problem 56
==========


   A googol (10^100) is a massive number: one followed by one-hundred zeros;
   100^100 is almost unimaginably large: one followed by two-hundred zeros.
   Despite their size, the sum of the digits in each number is only 1.

   Considering natural numbers of the form, a^b, where a, b < 100, what is
   the maximum digital sum?


   Answer: c22abfa379f38b5b0411bc11fa9bf92f
-}

import Data.Char

digitize :: Integer -> [Int]
digitize = map digitToInt . show

main :: IO ()
main = print $ maximum [sum (digitize (a^b)) | a <- [1..99]::[Integer], b <- [1..99]::[Integer]]