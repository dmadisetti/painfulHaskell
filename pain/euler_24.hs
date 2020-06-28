{- Project Euler
Problem 24
==========


   A permutation is an ordered arrangement of objects. For example, 3124 is
   one possible permutation of the digits 1, 2, 3 and 4. If all of the
   permutations are listed numerically or alphabetically, we call it
   lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

                       012   021   102   120   201   210

   What is the millionth lexicographic permutation of the digits 0, 1, 2, 3,
   4, 5, 6, 7, 8 and 9?


   Answer: 7f155b45cb3f0a6e518d59ec348bff84
-}


perm :: [a] -> [[a]]
perm [] = [[]]
perm xs = [y:zs | (y,ys) <- select xs, zs <- perm ys]
  where select []     = []
        select (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- select xs ]

main :: IO ()
main = print result
  where
    result :: Int
    result = read $ perm "0123456789" !! (1000000 - 1)
