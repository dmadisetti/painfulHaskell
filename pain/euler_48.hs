{- Project Euler
Problem 48
==========


   The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

   Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.


   Answer: 0829124724747ae1c65da8cae5263346
-}

import Data.HashMap
import Helpers (prime_factorize)

m :: Integer
m = 10000000000

-- Construct hashmap of factor counts
enumerate :: Int -> [Int] -> Map Int Int
enumerate n xs = Data.HashMap.map (*n) base
  where
    base = foldl aggregate init xs
    update old new = old + new
    aggregate :: Map Int Int -> Int -> Map Int Int
    aggregate m el = insertWith update el 1 m
    init :: Map Int Int
    init = empty

em b c m = c':em b c' m
  where
    c' = mod (b * c) m

modulo :: Integer -> Int -> Integer -> Integer
modulo b e m = last $ take e (em b 1 m)

moduloMap :: Int -> Int -> Integer -> Integer
moduloMap key val prev = mod (prev * (modulo (toInteger key) val m)) m

-- What's the point if this is faster
-- main = print $ flip mod m $ (+) 1 $ sum $ zipWith (^) range range
main :: IO ()
main = print $ flip mod m $ (+) 1 $ sum $ Prelude.map (foldWithKey moduloMap 1) $ zipWith enumerate range (Prelude.map (prime_factorize) range)
  where
    range = [2..1000]
