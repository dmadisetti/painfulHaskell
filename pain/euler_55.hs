{- Project Euler
Problem 55
==========


   If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

   Not all numbers produce palindromes so quickly. For example,

   349 + 943 = 1292,
   1292 + 2921 = 4213
   4213 + 3124 = 7337

   That is, 349 took three iterations to arrive at a palindrome.

   Although no one has proved it yet, it is thought that some numbers, like
   196, never produce a palindrome. A number that never forms a palindrome
   through the reverse and add process is called a Lychrel number. Due to the
   theoretical nature of these numbers, and for the purpose of this problem,
   we shall assume that a number is Lychrel until proven otherwise. In
   addition you are given that for every number below ten-thousand, it will
   either (i) become a palindrome in less than fifty iterations, or, (ii) no
   one, with all the computing power that exists, has managed so far to map
   it to a palindrome. In fact, 10677 is the first number to be shown to
   require over fifty iterations before producing a palindrome:
   4668731596684224866951378664 (53 iterations, 28-digits).

   Surprisingly, there are palindromic numbers that are themselves Lychrel
   numbers; the first example is 4994.

   How many Lychrel numbers are there below ten-thousand?

   NOTE: Wording was modified slightly on 24 April 2007 to emphasise the
   theoretical nature of Lychrel numbers.


   Answer: 077e29b11be80ab57e1a2ecabb7da330
-}

import Data.HashMap (Map, empty, lookup, insert)
import Helpers (isPalindrome)

import Data.Char

type Path = Map Integer Int

limit = 10000

compute cache head
   | head >= limit   = 0
   | otherwise       = hit + compute cache' (head + 1)
   where
      (cache', hit)  = look cache head 0

look :: Path -> Integer -> Int -> (Path, Int)
look p k d = get $ Data.HashMap.lookup k p
       where
         t = k + k'
         k' = read $ reverse (show k)
         s = show t
         check
            | isPalindrome s && k < limit   =  double 1 p
            | isPalindrome s                =  (p, 1)
            | d > 50           && k < limit   =  double 0 p
            | d > 50                          =  (p, 0)
            | k < limit                       =  double l p'
            | otherwise                       =  (p', l)
               where
                  (p', l) = look p t (d + 1)
                  double x q = (insert k' x (insert k x q), x)

         get (Just l)  = (p, l)
         get Nothing   = check

main :: IO ()
main = print $ 10000 - compute empty 0