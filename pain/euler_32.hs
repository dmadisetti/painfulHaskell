{- Project Euler
Problem 32
==========


   We shall say that an n-digit number is pandigital if it makes use of all
   the digits 1 to n exactly once; for example, the 5-digit number, 15234, is
   1 through 5 pandigital.

   The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
   multiplicand, multiplier, and product is 1 through 9 pandigital.

   Find the sum of all products whose multiplicand/multiplier/product
   identity can be written as a 1 through 9 pandigital.

   HINT: Some products can be obtained in more than one way so be sure to
   only include it once in your sum.

   Answer: 100f6e37d0b0564490a2ee27eff0660d
-}

import Data.Set (fromList)

viable :: [Integer]
viable = capture (reverse [1..9]) []
  where
    capture :: [Integer] -> [Integer] -> [Integer]
    capture [] [a,b,z,d,e,f,g,h,i]
      | valid         = [result]
      | valid'        = [result]
      | otherwise     = []
      where
        result = toNum [f,g,h,i]
        valid  = check [a,b,z] [d,e]
        valid' = check [a,b,z,d] [e]
        check x y = toNum x * toNum y == result
        toNum = sum . zipWith (*) (map (10^) [0..]) . reverse
    capture ks n = mconcat $ map gen ks
      where
        next k = k:n
        gen k = capture (filter (/=k) ks) (next k)


main :: IO ()
main = print $ sum $ fromList viable
