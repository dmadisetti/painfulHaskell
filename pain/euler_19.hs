{- Project Euler
Problem 19
==========


   You are given the following information, but you may prefer to do some
   research for yourself.

     • 1 Jan 1900 was a Monday.
     • Thirty days has September,
       April, June and November.
       All the rest have thirty-one,
       Saving February alone,
       Which has twenty-eight, rain or shine.
       And on leap years, twenty-nine.
     • A leap year occurs on any year evenly divisible by 4, but not on a
       century unless it is divisible by 400.

   How many Sundays fell on the first of the month during the twentieth
   century (1 Jan 1901 to 31 Dec 2000)?


   Answer: a4a042cf4fd6bfb47701cbc8a1653ada
-}

months :: [Int]
months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
months' :: [Int]
months' = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

sundays :: (Int, Int) -> Int -> (Int, Int)
sundays (d, s) y
  | mod y 4 == 0 && mod y 400 > 0 = compute months'
  | otherwise                     = compute months
  where
    compute :: [Int] -> (Int, Int)
    compute m = foldl next (d, s) m
      where
        next (d, s) b
          | d == 0     = (next', s + 1)
          | otherwise  = (next', s)
          where next'  = mod (d + b) 7

main :: IO ()
main = print $ snd $ foldl (sundays) (2, 0) range
  where
    range = [1901..2000]
