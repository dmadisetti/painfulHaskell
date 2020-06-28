{- Project Euler
Problem 49
==========


   The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
   increases by 3330, is unusual in two ways: (i) each of the three terms are
   prime, and, (ii) each of the 4-digit numbers are permutations of one
   another.

   There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
   primes, exhibiting this property, but there is one other 4-digit
   increasing sequence.

   What 12-digit number do you form by concatenating the three terms in this
   sequence?


   Answer: 0b99933d3e2a9addccbb663d46cbb592
-}

import           Data.HashMap (Map, elems, empty, insertWith, isSubmapOf)
import           Data.Maybe   (mapMaybe)
import           Data.Monoid
import           Helpers      (isPrime, primes)
import           Text.Printf  (printf)

comp h@(a:as) (b:bs)
  | x > 10000     = comp h []
  | isPrime x    = map show [a, b, x]:comp h bs
  | otherwise     = comp h bs
  where
    x = b * 2 - a
comp (_:a:as) [] = comp (a:as) as
comp _ _ = []

perm :: [String] -> Maybe String
perm ps
  | is_perm     = Just (mconcat ps)
  | otherwise   = Nothing
  where
    is_perm = getAll $ mconcat $ map (All . (==) r) reduced
    (r:reduced) = map (foldl aggregate empty) ps
    aggregate m el = insertWith (+) el 1 m

main :: IO ()
main = printf "%s" $ last $ mapMaybe perm $ comp h ps
  where
    h@(_:ps) = filter (> 1000) $ takeWhile (< 10000) primes
