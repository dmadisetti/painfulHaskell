{- Project Euler
Problem 50
==========


   The prime 41, can be written as the sum of six consecutive primes:

                          41 = 2 + 3 + 5 + 7 + 11 + 13

   This is the longest sum of consecutive primes that adds to a prime below
   one-hundred.

   The longest sum of consecutive primes below one-thousand that adds to a
   prime, contains 21 terms, and is equal to 953.

   Which prime, below one-million, can be written as the sum of the most
   consecutive primes?


   Answer: 73229bab6c5dc1c7cf7a4fa123caf6bc
-}

import Helpers (primes, is_prime)
import Data.HashMap (Map, isSubmapOf, empty, insertWith, elems)

t = 1000000

-- let the memoize work for you?
-- Lookup for each, instead of build up

comp :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Map Int [(Int, Int, Int)] -> Map Int [(Int, Int, Int)]
comp f@((i,a):as) g@((j,b):bs) h@((k,c):cs) m
  | a  > t     = m
  | x  > t     = comp as [] [] m
  | x' > t     = comp f bs [] m
  | is_prime x'    = comp f g cs (insertWith (++) x' [(i, j, k)] m)
  | otherwise      = comp f g cs m
  where
    x = a + b
    x' = x + c
comp f@(_:g@(_:h)) [] [] m = comp f g h m
comp f@(_:f') g@(_:h) [] m
  | h == []   = comp f' [] [] m
  | otherwise = comp f g h m
comp _ _ _ m = m

main :: IO ()
main = print $ 1 -- length $ comp f g h empty
  where
    f@(_:g@(_:h)) = takeWhile check $ zip [1..] primes
    check (_, n) = n < t
