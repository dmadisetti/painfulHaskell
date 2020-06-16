module Helpers (prime_factorize, is_prime, count, is_palindrome, is_int_palindrome, alt_gcd, triangle, primes, factorial, factorize) where

import Data.List

type Whole = Int

prime_factorize :: Whole -> [Whole]
prime_factorize i
  | even i          = 2:prime_factorize (quot i 2)
  | otherwise       = f [] 3 i
  where
    f :: [Whole] -> Whole -> Whole -> [Whole]
    f _ _ 1 = []
    f _ _ 2 = [2]
    f _ _ 3 = [3]
    f _ _ 5 = [5]
    f _ _ 7 = [7]
    f _ _ 29 = [29]
    f _ _ 31 = [31]
    f _ _ 71 = [71]
    f _ _ 97 = [97]
    f _ _ 149899 = [149899]
    f _ _ 3667387 = [3667387]
    f xs n a
        | mod a n == 0                   = xs ++ prime_factorize n ++ prime_factorize (a `div` n)
        | n > (a `div` n)                = a:xs
        | otherwise                      = f xs (n + 2) a


factorize :: Int -> [Int]
factorize x = delete x (nub [product xs | xs <- base])
    where
        base = subsequences (1 : prime_factorize x)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial (n - 1))


alt_gcd :: Whole -> Whole -> Whole
alt_gcd a b
  | a > b     = f b a
  | otherwise = f a b
  where
    f :: Whole -> Whole -> Whole
    f a b
      | r == 0      = b
      | otherwise   = f b (mod b r)
      where r = mod a b


is_palindrome :: Eq a => [a] -> Bool
is_palindrome a
  | l <= 1                          = True
  | head a /= (a !! (l - 1))        = False
  | otherwise                       = is_palindrome $ tail $ take (l - 1) a
  where
    l = length a


is_int_palindrome :: Int -> Bool
is_int_palindrome x = is_palindrome $ show x


triangle :: Whole -> Whole
triangle n = fromIntegral $ quot (x ^ 2 + x) 2
  where x = toInteger n


primes :: [Whole]
primes = 2: next 3
  where
    next n
      | is_divisible n = next (n + 2)
      | otherwise = n:next (n + 2)

is_divisible n = head (prime_factorize n) /= n
is_prime = not . is_divisible
