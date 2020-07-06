{- Project Euler
Problem 52
==========


   It can be seen that the number, 125874, and its double, 251748, contain
   exactly the same digits, but in a different order.

   Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
   contain the same digits.


   Answer: a420384997c8a1a93d5a84046117c2aa
-}

import           Data.HashMap (Map, insertWith, empty)

enumerate :: String -> Map Char Int
enumerate = foldl aggregate empty
  where
    aggregate m el = insertWith (+) el 1 m

main :: IO ()
main = print $ head [n| r <- map (10^) [2..],
                        n <- [r..2 * r - 1],
                        let en = enumerate $ show n,
                        5 == length (takeWhile (en == ) $ map (enumerate . show . (*) n) [2..6])]
