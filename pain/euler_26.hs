{- Project Euler
Problem 26
==========


   A unit fraction contains 1 in the numerator. The decimal representation of
   the unit fractions with denominators 2 to 10 are given:

     1/2  =  0.5
     1/3  =  0.(3)
     1/4  =  0.25
     1/5  =  0.2
     1/6  =  0.1(6)
     1/7  =  0.(142857)
     1/8  =  0.125
     1/9  =  0.(1)
     1/10 =  0.1

   Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can
   be seen that 1/7 has a 6-digit recurring cycle.

   Find the value of d < 1000 for which ^1/[d] contains the longest recurring
   cycle in its decimal fraction part.


   Answer: 6aab1270668d8cac7cef2566a1c5f569
-}

import           Data.HashMap (Map, insert, singleton, member, lookup)
import           Data.List (sort)
import           Data.Maybe

pow = (+) 1 . floor . logBase 10 . fromIntegral
remm l x = mod (10^((pow x) + l)) x
check :: Integer -> Maybe Integer
check x = check' (singleton (remm 0 x) 0) 1
  where
    check' :: Map Integer Integer -> Integer -> Maybe Integer
    check' m l
      | member 0 m   = Nothing
      | member y m   = (-) (l) <$> Data.HashMap.lookup y m
      | otherwise    = check' (insert y l m) (l + 1)
      where
        y = remm l x

main :: IO ()
main = print $ snd $ last $ sort $ zip (map check range) range
  where
    range = [1..1000]
