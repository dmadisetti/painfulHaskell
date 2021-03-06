{- Project Euler
Problem 45
==========


   Triangle, pentagonal, and hexagonal numbers are generated by the following
   formulae:

   Triangle     T[n]=n(n+1)/2    1, 3, 6, 10, 15, ...
   Pentagonal   P[n]=n(3n−1)/2   1, 5, 12, 22, 35, ...
   Hexagonal    H[n]=n(2n−1)     1, 6, 15, 28, 45, ...

   It can be verified that T[285] = P[165] = H[143] = 40755.

   Find the next triangle number that is also pentagonal and hexagonal.


   Answer: 30dfe3e3b286add9d12e493ca7be63fc
-}

hex :: [Int]
hex = [n * (2*n - 1) | n <- [1..]]

pentagonal :: [Int]
pentagonal = [quot (n * (3 * n -1)) 2 | n <- [1..]]

triangle :: [Int]
triangle = [quot (n * (n + 1)) 2 | n <- [1..]]

merge :: [Int]
merge = merge' hex pentagonal triangle
  where
  merge' hx@(h:hs) pt@(p:ps) tr@(t:ts)
      | h == p && p == t  = h:merge' hs ps ts
      | h > p             = merge' hx ps tr
      | h > t             = merge' hx pt ts
      | otherwise         = merge' hs pt tr

main :: IO ()
main = print $ last $ take 3 merge
