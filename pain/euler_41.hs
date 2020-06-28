{- Project Euler
Problem 41
==========


   We shall say that an n-digit number is pandigital if it makes use of all
   the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
   and is also prime.

   What is the largest n-digit pandigital prime that exists?


   Answer: d0a1bd6ab4229b2d0754be8923431404
-}

import           Helpers (isPrime)

viable :: [Int]
viable = capture (reverse [1..9]) 0 0
  where
    capture [] n i = []
    capture ks n i
      | (last ks - 1) == i      = search ++ [n]
      | otherwise                 = search
      where
        search = mconcat $ map gen ks
        next k = n * 10 + k
        gen k = capture (filter (/=k) ks) (next k) (max i k)


main :: IO ()
main = print $ head $ filter isPrime viable
