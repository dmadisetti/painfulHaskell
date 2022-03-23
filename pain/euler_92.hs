{- Project Euler
Problem 92
==========


   A number chain is created by continuously adding the square of the digits
   in a number to form a new number until it has been seen before.

   For example,

   44 → 32 → 13 → 10 → 1 → 1
   85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

   Therefore any chain that arrives at 1 or 89 will become stuck in an
   endless loop. What is most amazing is that EVERY starting number will
   eventually arrive at 1 or 89.

   How many starting numbers below ten million will arrive at 89?


   Answer: 6cee918c0612bccc2dac03d05e07035f
-}

import Data.IntMap (IntMap, empty, insert, toList, lookup, insertWith, elems, unionWith)
import qualified Data.IntMap (map)

import Helpers (primes)
import Data.Char

type Path = (IntMap Int, Int)
type Path' = IntMap Int

transform :: Int -> Int
transform k = sum $ map ((^2) . digitToInt) (show k)

hash h = foldl primeify 1 (show h)
   where
      primeify p x = p * (primes !! digitToInt x)


process' :: Path' -> Int -> Path
process' p'' = process'' (p'', 0)
   where
      process'' :: (Path', Int) -> Int -> (Path', Int)
      process'' (p, l) k = get $ Data.IntMap.lookup k' p
       where
         t = transform k
         t' = hash t
         k' = hash k
         check
            | t == 1  = double 0
            | t == 89  = double 1
            | otherwise = (insert k' l' p', l') 
               where 
                  (p', l') = process'' (p, 0) t
                  double x = (insert k' x $ insert t' x p, x)
         get (Just l')  = (p, l')
         get Nothing   = check

factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

compute limit = ans
   where
      fac d = factorial limit `quot` (factorial (limit - d) * factorial d)
      foldall :: Path -> Int -> Path
      foldall (p, i) k = (p', i + t')
         where
            (p', t') = recurse p 0 1 1 1 k
      (_p, ans) = foldl foldall (empty :: Path', 0) [9,8..1]
      recurse :: Path' -> Int -> Int -> Int -> Int -> Int -> Path
      recurse path head counts depth repeats k
         | depth > limit   = (path, 0)
         | otherwise       = (path'', adj + new)
         where
            foldp :: Path -> Int -> Path
            foldp (p, i) k' = (p', i + t')
               where
                  (p', t') = recurse p head' counts' (depth + 1) r k'
                  r
                     | k == k'   = repeats + 1
                     | otherwise = 1
            -- permutations + account for placements of 0s
            adj = (bit * counts') * fac depth
            head' = head + k * k
            (path', bit) = process' path head'
            (path'', new) = foldl foldp (path', 0) [1..k]
            counts' = (depth * counts) `quot` repeats

main :: IO ()
main = print $ compute 7