{- Project Euler
Problem 42
==========


   The n^th term of the sequence of triangle numbers is given by, t[n] =
   ½n(n+1); so the first ten triangle numbers are:

                    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

   By converting each letter in a word to a number corresponding to its
   alphabetical position and adding these values we form a word value. For
   example, the word value for SKY is 19 + 11 + 25 = 55 = t[10]. If the word
   value is a triangle number then we shall call the word a triangle word.

   Using [1]words.txt, a 16K text file containing nearly two-thousand common
   English words, how many are triangle words?


   Visible links
   1. words.txt
   Answer: 82aa4b0af34c2313a562076992e50aa3
-}

import           Data.Char    (ord)
import           Data.HashMap (Map, elems, empty, filterWithKey, insertWith)
import           Data.Maybe
import           Data.Monoid
import           System.IO

reorder :: Map Int Int -> Int -> Map Int Int
reorder nums el = insertWith (+) el 1 nums

isTriangular' n = k*(k+1) == n'
  where
    n' = (2 * n)
    k = floor $ sqrt $ fromIntegral n'
isTriangular _ = isTriangular'

extract_sum :: String -> Int
extract_sum =  sum . elems . filterWithKey (flip isTriangular) . foldl reorder empty . map sum . map numerize . parse
  where
   numerize = map ((+) (-64) . ord)
   parse = words . mapMaybe replace
     where
        replace n
          | n == ','      = Just ' '
          | bad           = Nothing
          | otherwise     = Just n
          where
            bad = getAny $ mconcat $ map (Any . (==) n) "[]\" "

main :: IO ()
main = do
  handle <- openFile "external/euler/files/words.txt" ReadMode
  contents <- hGetContents handle
  print $ extract_sum contents
  hClose handle
