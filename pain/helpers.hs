module Helpers (primeFactorize, isPrime, count, isPalindrome,
isIntPalindrome, altGcd, triangle, triangles, primes, factorial, factorize,
isDivisible, isPalindromeBase, composites, triplets) where

import           Data.Char (intToDigit)
import           Data.List
import           Numeric   (showIntAtBase)

type Whole = Int

primeFactorize :: Whole -> [Whole]
primeFactorize i
  | even i          = 2:primeFactorize (quot i 2)
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
    f _ _ 149899 = [149899]
    f _ _ 3667387 = [3667387]
    f xs n a
        | mod a n == 0       = xs ++ primeFactorize n ++ primeFactorize (a `div` n)
        | n > (a `div` n)    = a:xs
        | otherwise          = f xs (n + 2) a


factorize :: Int -> [Int]
factorize x = delete x (nub [product xs | xs <- base])
    where
        base = subsequences (1 : primeFactorize x)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


altGcd :: Whole -> Whole -> Whole
altGcd a b
  | a > b     = f b a
  | otherwise = f a b
  where
    f :: Whole -> Whole -> Whole
    f a b
      | r == 0      = b
      | otherwise   = f b (mod b r)
      where r = mod a b


isPalindrome :: Eq a => [a] -> Bool
isPalindrome a
  | l <= 1                  = True
  | head a /= last a        = False
  | otherwise               = isPalindrome $ tail $ take (l - 1) a
  where
    l = length a


isPalindromeBase :: Int -> Int -> Bool
isPalindromeBase 10 = isPalindrome . show
isPalindromeBase base = isPalindrome . flip (showIntAtBase base intToDigit) ""

isIntPalindrome :: Int -> Bool
isIntPalindrome =  isPalindromeBase 10


triangle :: Whole -> Whole
triangle n = fromIntegral $ quot (x ^ 2 + x) 2
  where x = toInteger n

triangles :: [Whole]
triangles = 1: next triangles 1 1
    where
      next _  1 1 = 3: next triangles 3 2
      next xs prev n = nth: next xs nth (n + 1)
        where
          nth = prev + n + 1

triplets :: [(Int, Int, Int)]
triplets = root:recurse [root]
  where
    root = (3, 4, 5)
    m (x, y, z) = (x + 2 * (y + z),  y + 2 * (x + z), 3 * z + 2 * (x + y))
    a (x, y, z) = m (x, -y, z)
    b (x, y, z) = m (x, y, z)
    c (x, y, z) = m (-x, y, z)
    recurse (x:q) = next ++ recurse (q ++ next)
      where
        next = map ($ x) [a, b, c]

primes :: [Whole]
primes = 2: 3 : next 3 0
  where
    next n d
      | d == 0        = next (n + 2) 1
      | isDivisible n = next (n + 2) d'
      | otherwise = n:next (n + 2) d'
      where
        d'
          | d == 1    = 2
          | otherwise = 0

composites :: [Whole]
composites = 4:6:next 8
  where
    next n
      | isPrime n = (n + 1):next (n + 2)
      | otherwise = n:next (n + 1)

isDivisible 1 = True
isDivisible n = head (primeFactorize n) /= n
isPrime = not . isDivisible
