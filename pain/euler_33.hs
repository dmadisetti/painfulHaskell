{- Project Euler
Problem 33
==========


   The fraction 49/98 is a curious fraction, as an inexperienced
   mathematician in attempting to simplify it may incorrectly believe that
   49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

   We shall consider fractions like, 30/50 = 3/5, to be trivial
   examples.

   There are exactly four non-trivial examples of this type of fraction, less
   than one in value, and containing two digits in the numerator and
   denominator.

   If the product of these four fractions is given in its lowest common
   terms, find the value of the denominator.


   Answer: f899139df5e1059396431415e770c6dd
-}

import           Data.Ratio

extract :: Int -> [Ratio Int]
extract x = [ra | a <- [1..x-1], b <- [a..x],
  let ra = (a * 10 + x) % (x * 10 + b), let rb = a % b, ra == rb]

main :: IO ()
main = print $ denominator $ product $ mconcat $ [extract x | x <- [1..9]]
