{- Project Euler
Problem 34
==========


   145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

   Find the sum of all numbers which are equal to the sum of the factorial of
   their digits.

   Note: as 1! = 1 and 2! = 2 are not sums they are not included.


   Answer: 60803ea798a0c0dfb7f36397d8d4d772
-}

import           Data.Char
import           Data.HashMap (Map, elems, empty, insertWith, mapWithKey,
                               singleton)
import           Helpers      (factorial)

limit = toInteger $ (+) 1 $ length $ show $ factorial 9

toNum :: Map Integer Integer -> Integer
toNum nums = sum $ mapWithKey pow nums
  where
    pow a b = b * factorial a

reorder :: Map Integer Integer -> Integer -> Map Integer Integer
reorder nums el = insertWith (+) el 1 nums

strMap :: String -> Map Integer Integer
strMap = foldl reorder empty . map (toInteger . digitToInt)

grow :: Integer -> Map Integer Integer -> [Integer]
grow k nums
  | (sum $ elems nums) > limit   = []
  | strMap n' == nums            = n:explosion
  | otherwise                    = explosion
  where
    n = toNum nums
    n' = show $ n
    explosion = mconcat $ map check (filter (>=k) [0..9])
    check k = grow k $ reorder nums k

main :: IO ()
main = print $ flip (-) 3 $ sum $ grow 0 empty
