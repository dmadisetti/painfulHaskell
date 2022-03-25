{- Project Euler
Problem 57
==========


   It is possible to show that the square root of two can be expressed as an
   infinite continued fraction.

              √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

   By expanding this for the first four iterations, we get:

   1 + 1/2 = 3/2 = 1.5
   1 + 1/(2 + 1/2) = 7/5 = 1.4
   1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
   1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

   The next three expansions are 99/70, 239/169, and 577/408, but the eighth
   expansion, 1393/985, is the first example where the number of digits in
   the numerator exceeds the number of digits in the denominator.

   In the first one-thousand expansions, how many fractions contain a
   numerator with more digits than denominator?


   Answer: b3e3e393c77e35a4a3f3cbd1e429b5dc
-}


progress :: Integer -> Integer -> (Integer, Integer)
progress a b = (2 * b + a, a + b)

ratio :: [(Integer, Integer)]
ratio = p (3, 2)
   where
      p x@(a, b) = x:(p y)
       where
         y@(a', b') = progress a b

met (a, b) = length (show a) > length (show b)

main :: IO ()
main = print $ length $ filter met (take 999 ratio)