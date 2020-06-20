{- Project Euler
Problem 39
==========


   If p is the perimeter of a right angle triangle with integral length
   sides, {a,b,c}, there are exactly three solutions for p = 120.

   {20,48,52}, {24,45,51}, {30,40,50}

   For which value of p ≤ 1000, is the number of solutions maximised?


   Answer: fa83a11a198d5a7f0bf77a1987bcd006
-}

import           Data.HashMap (Map, empty, foldWithKey, insertWith)
import           Helpers      (triplets)

accumulate :: Map Int Int -> Int -> Map Int Int
accumulate = inner 0
  where
    inner i m x
      | x' > 1000    = m
      | otherwise   = inner i' m' x
      where
        i' = i + 1
        m' = insertWith (+) x' 1 m
        x' = x * i'

greedy :: Int -> Int -> (Int, Int) -> (Int, Int)
greedy k v o@(ok, ov)
  | v > ov     = (k, v)
  | otherwise  = o

main :: IO ()
main = print $ fst $
              foldWithKey greedy (0, 0) $
              foldl accumulate empty $
              takeWhile (<5000) $ map (\(a, b, c) -> a + b + c) triplets
