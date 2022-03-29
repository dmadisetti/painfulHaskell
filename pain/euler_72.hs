{- Project Euler
Problem 72
==========


   Consider the fraction, n/d, where n and d are positive integers. If n<d
   and HCF(n,d)=1, it is called a reduced proper fraction.

   If we list the set of reduced proper fractions for d ≤ 8 in ascending
   order of size, we get:

   1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
                          5/7, 3/4, 4/5, 5/6, 6/7, 7/8

   It can be seen that there are 21 elements in this set.

   How many elements would be contained in the set of reduced proper
   fractions for d ≤ 1,000,000?


   Answer: 0384fb529dc651fe0f460acff3e9ac5d
-}

import   Data.HashMap
import   Helpers (primeFactorize)

-- Construct hashmap of factor counts
enumerate :: [Int] -> Map Int Int
enumerate xs = base
  where
    base = foldl aggregate init xs
    update old new = old + new
    aggregate :: Map Int Int -> Int -> Map Int Int
    aggregate m el = insertWith update el 1 m
    init :: Map Int Int
    init = empty

-- counts distinct factors
totient :: Int -> Int
totient n = foldWithKey folder 1 factors
   where
      factors = enumerate $ primeFactorize n
      folder k v t = k^(v - 1) * (k - 1) * t

main :: IO ()
main = print $ sum $ Prelude.map totient x
   where
      x = [2..1000000]