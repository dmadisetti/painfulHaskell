{- Project Euler
Problem 9
=========


   A Pythagorean triplet is a set of three natural numbers, a < b < c, for
   which,

                                a^2 + b^2 = c^2

   For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

   There exists exactly one Pythagorean triplet for which a + b + c = 1000.
   Find the product abc.


   Answer: 24eaa9820350012ff678de47cb85b639
-}

f magic = head [a * b * c | a<-[1..magic - 2], b <- [a..magic - a - 1], let c = magic - a - b, a^2 + b^2 == c^2]


main :: IO ()
main = print $ f 1000
