{- Project Euler
Problem 30
==========


   Surprisingly there are only three numbers that can be written as the sum
   of fourth powers of their digits:

     1634 = 1^4 + 6^4 + 3^4 + 4^4
     8208 = 8^4 + 2^4 + 0^4 + 8^4
     9474 = 9^4 + 4^4 + 7^4 + 4^4

   As 1 = 1^4 is not a sum it is not included.

   The sum of these numbers is 1634 + 8208 + 9474 = 19316.

   Find the sum of all the numbers that can be written as the sum of fifth
   powers of their digits.


   Answer: 27a1779a8a8c323a307ac8a70bc4489d
-}

import           Data.Char
import           Data.HashMap (Map, elems, empty, insertWith, mapWithKey,
                               singleton)

limit = (+) 1 $ length $ show $ 9^5

toNum :: Map Int Int -> Int
toNum nums = sum $ mapWithKey pow nums
  where
    pow a b = b * a ^ 5

reorder :: Map Int Int -> Int -> Map Int Int
reorder nums el = insertWith (+) el 1 nums

strMap :: String -> Map Int Int
strMap = foldl reorder empty . map digitToInt

grow :: Int -> Map Int Int -> [Int]
grow k nums
  | sum (elems nums) > limit   = []
  | strMap n' == nums     = n:explosion
  | otherwise             = explosion
  where
    n = toNum nums
    n' = show n
    explosion = mconcat $ map check (filter (>=k) [0..9])
    check k = grow k $ reorder nums k

main :: IO ()
main = print $ flip (-) 1 $ sum $ grow 0 empty
