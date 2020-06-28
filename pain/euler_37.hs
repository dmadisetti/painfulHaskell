{- Project Euler
Problem 37
==========


   The number 3797 has an interesting property. Being prime itself, it is
   possible to continuously remove digits from left to right, and remain
   prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
   right to left: 3797, 379, 37, and 3.

   Find the sum of the only eleven primes that are both truncatable from left
   to right and right to left.

   NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


   Answer: cace46c61b00de1b60874936a093981d
-}

import           Data.Char
import           Data.Set  (fromList, intersection)
import           Helpers   (isPrime, primes)


growable = "123579"

grow :: (Int -> Char -> Int) -> String -> Int -> [Int]
grow expand (k:ks) n
  | isPrime n'     = n': grow expand growable n' ++ grow expand ks n
  | otherwise       = grow expand ks n
  where
    n' = expand n k

grow _ [] n = []

toInt :: String -> Int
toInt n = sum $ zipWith numerize (reverse n) [0..l]
  where
    l = length n - 1
    numerize c p = digitToInt c * 10 ^ p

expandLeft n k =  toInt $ k:show n
expandRight n k = toInt $ show n ++ [k]

process f = fromList $
                mconcat $
                    map (grow f growable) (takeWhile (<10) primes)

left = process expandLeft
right = process expandRight

main :: IO ()
main = print $ sum $ intersection left right
