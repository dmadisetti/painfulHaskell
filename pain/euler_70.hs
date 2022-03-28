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
import   Data.HashMap
import   Helpers (primeFactorize, primes)
import   GHC.Float

factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

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
   | v < v'    = a
   | otherwise = b

isPermutation (i, t) = i' == t'
   where
      i' = f i
      t' = f t
      f = sort . show

bruteforce :: IO ()
bruteforce = print $ foldl greedy (0, 100) $ Prelude.map ratio $ Prelude.filter isPermutation $ zip x $ Prelude.map totient x
   where
      x = [5985511..10000000]
      ratio (i, t) = (i, int2Float i / int2Float t)

-- knowing it will be a 2 factor prime
main = print $ attempt lower upper (5985511 `quot` pivot) (0, 2)
   where
      attempt :: [Int] -> [Int] -> Int -> (Int, Float) -> Int
      attempt (l:ls) k@(u:u':us) b r@(i, v)
         | u * l > 10000000   = attempt ls k b r
         | l < b              = attempt lower (u':us) (5985511 `quot` u') r
         | perm && smaller    = attempt ls k b (l*u, v')
         | otherwise          = attempt ls k b r
         where
            smaller = v' < v
            v' = ratio (l*u) ((l - 1)*(u - 1))
            perm = isPermutation (l*u, (l - 1) * (u - 1))
      attempt _ _ _ (i, _) = i
      ratio i t = int2Float i / int2Float t
      (lower, upper) = cleave ([], takeWhile (< 2 * pivot) primes)
      pivot = round $ sqrt 5985511
      cleave (a, (x:xs))
         | x > pivot   = (x:a, xs)
         | otherwise   = cleave (x:a, xs)
