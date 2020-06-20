{- Project Euler
Problem 38
==========


   Take the number 192 and multiply it by each of 1, 2, and 3:

     192 × 1 = 192
     192 × 2 = 384
     192 × 3 = 576

   By concatenating each product we get the 1 to 9 pandigital, 192384576. We
   will call 192384576 the concatenated product of 192 and (1,2,3)

   The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
   and 5, giving the pandigital, 918273645, which is the concatenated product
   of 9 and (1,2,3,4,5).

   What is the largest 1 to 9 pandigital 9-digit number that can be formed as
   the concatenated product of an integer with (1,2, ... , n) where n > 1?


   Answer: f2a29ede8dc9fae7926dc7a4357ac25e
-}

import           Data.Char  (digitToInt, ord)
import           Data.List  (nub)
import           Data.Maybe

viable :: [Int]
viable = capture [1..9] 0
  where
    capture ks n
      | n > 1000   = [n]
      | otherwise  = [n] ++ (mconcat $ map gen ks)
      where
        next k = n * 10 + k
        gen k = capture (filter (/=k) ks) (next k)


toInt :: [Char] -> Int
toInt n = sum $ zipWith numerize (reverse n) [0..l]
  where
    l = (length n) - 1
    numerize c p = (digitToInt c) * 10 ^ p


pandigit :: Int -> Maybe Int
pandigit n = expand "" 1
  where
  expand s k
    | l > 9       = Nothing
    | invalid s   = Nothing
    | l == 9      = Just $ toInt s
    | otherwise   = expand s' k'
    where
      l = length s
      s' = s ++ show (n * k)
      k' = k + 1
  invalid s = sized || zeroed
    where
      sized = length s /= (length $ nub $ s)
      zeroed = elem 48 (map ord s)


main :: IO ()
main = print $ maximum $ mapMaybe pandigit viable
