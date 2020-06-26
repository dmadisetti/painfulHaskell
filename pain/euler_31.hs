{- Project Euler
Problem 31
==========


   In England the currency is made up of pound, £, and pence, p, and there
   are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

   It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

   How many different ways can £2 be made using any number of coins?


   Answer: 142dfe4a33d624d2b830a9257e96726d
-}

growable = [1, 2, 5, 10, 20, 50, 100]

grow :: Int -> Int -> Int
grow n k
  | n < 0              = 0
  | n == 0             = 1
  | otherwise          = explosion
  where
    explosion = sum $  map check (filter (<=k) growable)
    check k = grow (n - k) k

main :: IO ()
main = print $ (+) 1 $ grow 200 200
