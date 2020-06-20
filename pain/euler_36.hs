{- Project Euler
Problem 36
==========


   The decimal number, 585 = 1001001001[2] (binary), is palindromic in both
   bases.

   Find the sum of all numbers, less than one million, which are palindromic
   in base 10 and base 2.

   (Please note that the palindromic number, in either base, may not include
   leading zeros.)


   Answer: 0e175dc2f28833885f62e7345addff03
-}

import Helpers (is_palindrome_base, is_int_palindrome)

limit = 1000000
growable = [0..9]


grow :: Int -> Int -> Int -> [Int]
grow d k n
  | d > 3              = []
  | n > limit          = []
  | palindromic        = n:explosion
  | otherwise          = explosion
  where
    palindromic = (is_palindrome_base 2 n)
    explosion = mconcat $ map check growable
    check k = grow (d + 1) k (expand d n k)


expand :: Int -> Int -> Int -> Int
expand d n k = read $ k':n' ++ [k']
  where
    n'
      | n == 0    = take d (repeat '0')
      | otherwise = extra ++ n''
      where
        extra = takeWhile (== '0') (reverse n'')
        n'' = show n
    [k'] = show k


main :: IO ()
main = print $ sum $ mconcat $ map (grow 0 0) [0..9]
