{- Project Euler
Problem 14
==========


   The following iterative sequence is defined for the set of positive
   integers:

   n → n/2 (n is even)
   n → 3n + 1 (n is odd)

   Using the rule above and starting with 13, we generate the following
   sequence:

                   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

   It can be seen that this sequence (starting at 13 and finishing at 1)
   contains 10 terms. Although it has not been proved yet (Collatz Problem),
   it is thought that all starting numbers finish at 1.

   Which starting number, under one million, produces the longest chain?

   NOTE: Once the chain starts the terms are allowed to go above one million.


   Answer: 5052c3765262bb2c6be537abd60b305e
-}

import Data.HashMap (Map, empty, lookup, insert)


type Path = Map Int Int

collatz :: Int -> Int
collatz 1 = 1
collatz n
  | even n      = quot n 2
  | otherwise   = 3*n + 1

limit = 1000000

compute cache (v, greedy) head
   | head >= limit   = v
   | otherwise       = compute cache' test (head + 1)
   where
      (cache', depth)  = look cache head 0
      test
        | greedy < depth = (head, depth)
        | otherwise      = (v, greedy)

look :: Path -> Int -> Int -> (Path, Int)
look p k d = get $ Data.HashMap.lookup k p
       where
          k' = collatz k
          check
            | k' == 1        = (p,  1)
            | k  > 1000000   = (p', l + 1)
            | otherwise      = (insert k (l + 1) p', l + 1)
               where
                  (p', l) = look p k' (d + 1)
          get (Just l)  = (p, l)
          get Nothing   = check

main :: IO ()
main = print $ compute empty (1, 1) 1
