{- Project Euler
Problem 17
==========


   If the numbers 1 to 5 are written out in words: one, two, three, four,
   five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

   If all the numbers from 1 to 1000 (one thousand) inclusive were written
   out in words, how many letters would be used?

   NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
   forty-two) contains 23 letters and 115 (one hundred and fifteen) contains
   20 letters. The use of "and" when writing out numbers is in compliance
   with British usage.


   Answer: 6a979d4a9cf85135408529edc8a133d0
-}

cosmetize :: Int -> Int
cosmetize 1 = length "one"
cosmetize 2 = length "two"
cosmetize 3 = length "three"
cosmetize 4 = length "four"
cosmetize 5 = length "five"
cosmetize 6 = length "six"
cosmetize 7 = length "seven"
cosmetize 8 = length "eight"
cosmetize 9 = length "nine"
cosmetize 10 = length "ten"
cosmetize 11 = length "eleven"
cosmetize 12 = length "twelve"
cosmetize 13 = length "thirteen"
cosmetize 15 = length "fifteen"
cosmetize 18 = length "eighteen"
cosmetize 20 = length "twenty"
cosmetize 30 = length "thirty"
cosmetize 40 = length "forty"
cosmetize 50 = length "fifty"
cosmetize 60 = length "sixty"
cosmetize 70 = length "seventy"
cosmetize 80 = length "eighty"
cosmetize 90 = length "ninety"
cosmetize 100 = length "hundred"
cosmic :: [Int] -> Int
cosmic ns = sum $ map cosmetize ns

main :: IO ()
main = print $
        100 * cosmic ones +
        (cosmetize 100) * 900 +
        9 * 99 * and +
        10 * perHundred + thousand
  where
    ones = [1..9]
    weird = [10..13] ++ [15, 18]
    teens = [4, 9] ++ [6, 7]
    tens = [20, 30..90]
    and = length "and"
    teen = length "teen"
    thousand = length "onethousand"
    perHundred = cosmic (ones ++ weird) +
          cosmic teens + teen * (length teens) +
          10 * cosmic tens +
          (length tens) * cosmic ones
