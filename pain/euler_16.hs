{- Project Euler
Problem 16
==========


   2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

   What is the sum of the digits of the number 2^1000?


   Answer: 6a5889bb0190d0211a991f47bb19a777
-}

import Data.Char

main :: IO ()
main = print $ sum $ map digitToInt (show $ 2 ^ 1000)
