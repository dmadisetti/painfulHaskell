{- Project Euler
Problem 97
==========


   The first known prime found to exceed one million digits was discovered in
   1999, and is a Mersenne prime of the form 2^6972593−1; it contains exactly
   2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p−1,
   have been found which contain more digits.

   However, in 2004 there was found a massive non-Mersenne prime which
   contains 2,357,207 digits: 28433×2^7830457+1.

   Find the last ten digits of this prime number.


   Answer: 68c8c919526039022b923a72d5cc12b1
-}

import Data.Char

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs
                           else []

assemble = read -- . map intToDigit

pow2ending :: Integer -> [String]
pow2ending m = ending 4 0
   where
      ending a i = [e]:ending (a * 5) (i + 1)
        where
          e = reverse (show (2 ^ (m `mod` a))) !! i

main :: IO ()
main = print $ (p * k + 1) `mod` (m * 10)
   where
      m = 10^9
      p = assemble $ last $ takeUntil ((>) m . assemble) s
      s = (scanl1 (flip (++)) $ pow2ending 7830457) :: [String]
      k = 28433