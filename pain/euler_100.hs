{- Project Euler
Problem 100
===========


   If a box contains twenty-one coloured discs, composed of fifteen blue
   discs and six red discs, and two discs were taken at random, it can be
   seen that the probability of taking two blue discs, P(BB) =
   (15/21)×(14/20) = 1/2.

   The next such arrangement, for which there is exactly 50% chance of taking
   two blue discs at random, is a box containing eighty-five blue discs and
   thirty-five red discs.

   By finding the first arrangement to contain over 10^12 = 1,000,000,000,000
   discs in total, determine the number of blue discs that the box would
   contain.


   Answer: 21156e3acc4ca35b7a318c541a0648d5
-}

import Control.Monad

-- A bit of alegebra, we can get the form of solutions
-- Generate and take until we meet the criteria.
-- Double because Float doesn't provide enough precision
solutions :: [[Double]]
solutions = next 1
   where
      next a = sol a:next (a + 1)
      sol :: Double -> [Double]
      sol m = [n, k]
         where
            k = (b**m - a**m) / c
            n = (b**m + a**m) / 4 + k + 0.5
            a = 3 - s22
            b = 3 + s22
            c = 2 * s22
            s22 = 2 * sqrt 2



main :: IO ()
main = print $ round n
   where [n, k] =  fst . last . ap zip tail $ takeWhile ((>) 10e12 . sum) solutions