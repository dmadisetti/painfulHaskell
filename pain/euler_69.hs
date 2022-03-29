{- Project Euler
Problem 69
==========


   Euler's Totient function, φ(n) [sometimes called the phi function], is
   used to determine the number of numbers less than n which are relatively
   prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine
   and relatively prime to nine, φ(9)=6.

   ┌────┬──────────────────┬──────┬───────────┐
   │ n  │ Relatively Prime │ φ(n) │ n/φ(n)    │
   ├────┼──────────────────┼──────┼───────────┤
   │ 2  │ 1                │ 1    │ 2         │
   ├────┼──────────────────┼──────┼───────────┤
   │ 3  │ 1,2              │ 2    │ 1.5       │
   ├────┼──────────────────┼──────┼───────────┤
   │ 4  │ 1,3              │ 2    │ 2         │
   ├────┼──────────────────┼──────┼───────────┤
   │ 5  │ 1,2,3,4          │ 4    │ 1.25      │
   ├────┼──────────────────┼──────┼───────────┤
   │ 6  │ 1,5              │ 2    │ 3         │
   ├────┼──────────────────┼──────┼───────────┤
   │ 7  │ 1,2,3,4,5,6      │ 6    │ 1.1666... │
   ├────┼──────────────────┼──────┼───────────┤
   │ 8  │ 1,3,5,7          │ 4    │ 2         │
   ├────┼──────────────────┼──────┼───────────┤
   │ 9  │ 1,2,4,5,7,8      │ 6    │ 1.5       │
   ├────┼──────────────────┼──────┼───────────┤
   │ 10 │ 1,3,7,9          │ 4    │ 2.5       │
   └────┴──────────────────┴──────┴───────────┘

   It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.

   Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.


   Answer: bf08b01ead83cbd62a9839ca1cf35ada
-}

import   Data.HashMap
import   Helpers (primeFactorize)
import   GHC.Float

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

greedy :: (Int, Float) -> (Int, Float) -> (Int, Float)
greedy a@(i, v) b@(_,v')
   | v > v'    = a
   | otherwise = b

main :: IO ()
main = print $ fst $ foldl greedy (0, 0) $ zip x $ Prelude.map ratio x
   where
      x = [1..1000000]
      ratio n = int2Float n / int2Float (totient n)