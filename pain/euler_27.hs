{- Project Euler
Problem 27
==========


   Euler discovered the remarkable quadratic formula:

                                  n² + n + 41

   It turns out that the formula will produce 40 primes for the consecutive
   values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41
   is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
   divisible by 41.

   The incredible formula  n² − 79n + 1601 was discovered, which produces 80
   primes for the consecutive values n = 0 to 79. The product of the
   coefficients, −79 and 1601, is −126479.

   Considering quadratics of the form:

     n² + an + b, where |a| < 1000 and |b| < 1000

     where |n| is the modulus/absolute value of n
     e.g. |11| = 11 and |−4| = 4

   Find the product of the coefficients, a and b, for the quadratic
   expression that produces the maximum number of primes for consecutive
   values of n, starting with n = 0.


   Answer: 69d9e3218fd7abb6ff453ea96505183d
-}

import           Helpers (is_prime, primes)

viableTill :: Int -> Int -> Int
viableTill a b = length $ takeWhile (is_prime . f) [1..]
  where
    f x = x^2 + a * x + b

main :: IO ()
main = print $ snd $ maximum [(v, a * b) | b <- ps, a <- [-l, 2-l..l], a ^ 2 - 4 * b <= 0, let v = viableTill a b]
  where
    l = 999
    ps = takeWhile (<= l) primes
