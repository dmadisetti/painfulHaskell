{- Project Euler
Problem 99
==========


   Comparing two numbers written in index form like 2^11 and 3^7 is not
   difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.

   However, confirming that 632382^518061 > 519432^525806 would be much more
   difficult, as both numbers contain over three million digits.

   Using [1]base_exp.txt, a 22K text file containing one thousand lines with
   a base/exponent pair on each line, determine which line number has the
   greatest numerical value.

   NOTE: The first two lines in the file represent the numbers in the example
   given above.


   Visible links
   1. base_exp.txt
   Answer: 1ecfb463472ec9115b10c292ef8bc986
-}

import           Data.List.Split
import           System.IO

parse = map _split . lines
   where
      _split :: String -> Float
      _split = last2 . map read . splitOn ","
         where
            last2 [a,b] = b * log a

greedyMax :: (Int, Float) -> (Int, Float) -> (Int, Float)
greedyMax a@(w, score) b@(m, score')
   | score' > score        = b
   | otherwise             = a

main :: IO ()
main = do
  handle <- openFile "external/euler/files/base_exp.txt" ReadMode
  contents <- hGetContents handle
  let parsed = parse contents
  print $ fst $ foldl greedyMax (0, 0) $ zip [1..length parsed] parsed
  hClose handle