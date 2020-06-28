{- Project Euler
Problem 50
==========


   The prime 41, can be written as the sum of six consecutive primes:

                          41 = 2 + 3 + 5 + 7 + 11 + 13

   This is the longest sum of consecutive primes that adds to a prime below
   one-hundred.

   The longest sum of consecutive primes below one-thousand that adds to a
   prime, contains 21 terms, and is equal to 953.

   Which prime, below one-million, can be written as the sum of the most
   consecutive primes?


   Answer: 73229bab6c5dc1c7cf7a4fa123caf6bc
-}

import           Data.Maybe (mapMaybe)
import           Helpers    (isPrime, primes)

l = 1000000

f sigma l' delta (i, s)
  | i + delta >= l' ||  ans > l  = Nothing
  | isPrime ans = Just ans
  | otherwise  = Nothing
  where
    ans = (sigma !! (i+delta)) - s

main :: IO ()
main = print $ head $ mapMaybe check processes
  where
    check p
      | null r  = Nothing
      | otherwise = Just (head r)
      where
        r = mapMaybe p zipped
    processes = map (f cumulative l') [l', l'-1..]
    l' = length cumulative
    zipped = zip [0..] cumulative
    cumulative = takeWhile (<l) $ scanl1 (+) ps
    ps = takeWhile (<l) primes
