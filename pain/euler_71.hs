{- Project Euler
Problem 71
==========


   Consider the fraction, n/d, where n and d are positive integers. If n<d
   and HCF(n,d)=1, it is called a reduced proper fraction.

   If we list the set of reduced proper fractions for d ≤ 8 in ascending
   order of size, we get:

   1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
                          5/7, 3/4, 4/5, 5/6, 6/7, 7/8

   It can be seen that 2/5 is the fraction immediately to the left of 3/7.

   By listing the set of reduced proper fractions for d ≤ 1,000,000 in
   ascending order of size, find the numerator of the fraction immediately to
   the left of 3/7.


   Answer: 71f38fa2f04db30be52f883d583bfd6f
-}

import Data.Ratio

{-
x = np.arange(1,np.floor(1e6/7) + 2)
y = (3*x - 1)/(7*x)

v = np.argmin(abs(y - 3/7))
3*v,7*v,(3*v - 1)/(7*v)
-}
main :: IO ()
main = print $ num $ fst $ foldl greedy (0, 1.0) $ zip range $ map near range
   where
      range = [1..k]
      greedy a@(_, r) (i, v)
         | r' < r     = (i, r')
         | otherwise  = a
         where
            r' = r37 - v
      r37 = 3 % 7
      k :: Int
      k = floor (1000000.0/7.0)
      near x = num x % (7*x)
      num x = 3*x - 1