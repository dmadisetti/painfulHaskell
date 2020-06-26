{- Project Euler
Problem 40
==========


   An irrational decimal fraction is created by concatenating the positive
   integers:

                     0.123456789101112131415161718192021...

   It can be seen that the 12^th digit of the fractional part is 1.

   If d[n] represents the n^th digit of the fractional part, find the value
   of the following expression.

      d[1] × d[10] × d[100] × d[1000] × d[10000] × d[100000] × d[1000000]


   Answer: 6f3ef77ac0e3619e98159e9b6febf557
-}

import Data.Char

dive :: Int -> Int
dive x' = recurse 0 0
  where
    x = x' - 1
    recurse index power
      | (index' - x) < 0   = recurse index' (power + 1)
      | otherwise          = digitToInt $ (show $ (+) (10^power) $ quot (x - index) (power + 1)) !! (mod (x - index) (power + 1))
      where
        index' = index + (power + 1) * 9 * 10^power


main :: IO ()
main = print $ product $ map dive (map (10^) [0..6])
