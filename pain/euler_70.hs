{- Project Euler
Problem 70
==========


   Euler's Totient function, φ(n) [sometimes called the phi function], is
   used to determine the number of positive numbers less than or equal to n
   which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are
   all less than nine and relatively prime to nine, φ(9)=6.
   The number 1 is considered to be relatively prime to every positive
   number, so φ(1)=1.

   Interestingly, φ(87109)=79180, and it can be seen that 87109 is a
   permutation of 79180.

   Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n
   and the ratio n/φ(n) produces a minimum.


   Answer: 1884dde67ced589082c8b7043abce181
-}


import   Data.Sort
import   Helpers (primes)
import   GHC.Float

isPermutation (i, t) = i' == t'
   where
      i' = f i
      t' = f t
      f = sort . show

-- knowing it will be a 2 factor prime
main = print $ attempt lower lower upper (feasible `quot` pivot) (0, 2)
   where
      limit    = 10000000
      feasible = 4869943 -- from solving x = (log_10(limit) - 1) exp(gamma) log log x
      attempt ::[Int] -> [Int] -> [Int] -> Int -> (Int, Float) -> Int
      attempt pl (l:ls) k@(u:u':us) b r@(i, v)
         | l < b                 = maybeNext
         | isPermutation (n, t)  = attempt ls' ls' us' b' (n, v')
         | otherwise             = attempt pl ls k b r
         where
            maybeNext
               | l' < b'       = i
               | otherwise     = attempt ls' ls' us' b' r
            l'  = head ls'
            ls' = Prelude.filter ((>) limit . (*) u') (u:pl)
            us' = u':us
            -- our boundary is met when we cannot do better than the current ratio,
            -- or we are out of the feasible range
            b' = max (feasible `quot` u') $ floor $ 1 / (1 - ratio u' (u' - 1) / v)
            v' = ratio n t
            t = (l - 1) * (u - 1)
            n = l * u
      attempt _ _ _ _ (i, _) = i
      ratio n t = int2Float n / int2Float t
      (lower, upper) = cleave ([], primes)
      pivot = round $ sqrt $ int2Float feasible
      cleave (a, x:xs)
         | x > pivot   = (a, x:xs)
         | otherwise   = cleave (x:a, xs)
